/*
 * formatproc for repl command -- extract tex/plain only.
 *
 * To use <this-program>, you need to specify this formater as follow.
 *
 * in ~/.mh_profile
 *	repl:	... -filter <filter-file>
 *	formatproc:	absolute path of <this-program>
 *
 * in ~/<Path>/<filter-file>	default: mhl.{forward,reply}
 *	...
 *	body:<any spec you want>,\
 *	format,\
 *	formatarg=":=HDRT=:%{content-type}:=HDRE=:%{content-transfer-encoding}"
 *
 * above setting make <this-program> have an argument specifing
 * `Content-Type' field value and 'Content-Transfer-Encodig' field value
 * specified in the message header.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#if !defined(__FreeBSD__)
#define iconv_canonicalize(x) (x)
#define __USE_GNU
#endif
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <errno.h>
#include <langinfo.h>
#include <locale.h>
#include <iconv.h>
#define ICONV_NULL	((iconv_t)(-1))

#define	HDR_TYPE_STR		":=HDRT=:"
#define	HDR_TRENCODE_STR	":=HDRE=:"

/* output charset */
#define OUT_CHARSET	"euc-jp"

static const char *out_charset = OUT_CHARSET;

/*
 * content manipulation stuff =========================================
 */
#define MAX_NEST	10

struct {
  char *bndry;	/* boundary string */
  int bndrylen;
  iconv_t ict;	/* charset --> out_charset */
  int flag;
#define	_QP	1
#define	_B64	2
} msg_content[MAX_NEST], *msg_top = msg_content;
int msg_nest_cc = MAX_NEST-1;

static void
push_content(char *tr_encode, char *csname, char *bndry)
{
  if (--msg_nest_cc  <= 0) {
    fprintf(stderr, "msg content nesting too deep!\n");
    exit(1);
  }
  msg_top++;
  msg_top->bndry = NULL;
  msg_top->bndrylen = 0;
  msg_top->ict = ICONV_NULL;
  msg_top->flag = 0;

  if (bndry && *bndry) {
    msg_top->bndry = bndry;
    msg_top->bndrylen = strlen(bndry);
  }

  msg_top->flag = 0;
  if (tr_encode && *tr_encode) {
    if (!strcasecmp(tr_encode, "quoted-printable"))
      msg_top->flag = _QP;
    else if (!strcasecmp(tr_encode, "base64"))
      msg_top->flag = _B64;
  }

  if (csname && *csname) {
    if (!strcasecmp(csname, "guess")) csname = "iso-2022-jp";
    if (strcasecmp(csname, "iso-8859-1") &&
	strcasecmp(csname, "us-ascii") &&
	strcasecmp(csname, "ascii") &&
	strcasecmp(csname, out_charset) ) {
    if ((msg_top->ict = iconv_open(out_charset, csname)) == ICONV_NULL)
      fprintf(stderr, "iconv: can't convert %s to %s\n", csname, out_charset);
    }
  }
}

static void
pop_content()
{
  if (++msg_nest_cc > MAX_NEST-1) {
    fprintf(stderr, "msg content underflow\n");
    exit(1);
  }
  if (msg_top->bndry) (void)free(msg_top->bndry);
  if (msg_top->ict != ICONV_NULL) (void)iconv_close(msg_top->ict);
  msg_top--;
}

static void
set_content(char *tr_encode, char *csname)
{
  msg_top->flag = 0;
  if (tr_encode && *tr_encode) {
    if (!strcasecmp(tr_encode, "quoted-printable"))
      msg_top->flag = _QP;
    else if (!strcasecmp(tr_encode, "base64"))
      msg_top->flag = _B64;
  }

  if (msg_top->ict != ICONV_NULL)
    (void) iconv_close(msg_top->ict);
  msg_top->ict = ICONV_NULL;

  if (csname && *csname) {
    if (!strcasecmp(csname, "guess")) csname = "iso-2022-jp";
    if (strcasecmp(csname, "iso-8859-1") &&
	strcasecmp(csname, "us-ascii") &&
	strcasecmp(csname, "ascii") &&
	strcasecmp(csname, out_charset) ) {
    if ((msg_top->ict = iconv_open(out_charset, csname)) == ICONV_NULL)
      fprintf(stderr, "iconv: can't convert %s to %s\n", csname, out_charset);
    }
  }
}

static void
unset_content()
{
  msg_top->flag = 0;
  if (msg_top->ict != ICONV_NULL)
    (void) iconv_close(msg_top->ict);
  msg_top->ict = ICONV_NULL;
}

#define content_empty()	(msg_nest_cc == (MAX_NEST - 1))
#define	do_b64()	((msg_top->flag&_B64) == _B64)
#define	do_qp()		((msg_top->flag&_QP) == _QP)

/*
 * base64 decoding stuff ==============================================
 */
/* ==== sbr/fmt_rfc2047.c ==== */
static signed char index_64[] = {
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,62,-1,-1,-1,63,
    52,53,54,55,56,57,58,59, 60,61,-1,-1,-1, 0,-1,-1,
    -1, 0, 1, 2, 3, 4, 5, 6,  7, 8, 9,10,11,12,13,14,
    15,16,17,18,19,20,21,22, 23,24,25,-1,-1,-1,-1,-1,
    -1,26,27,28,29,30,31,32, 33,34,35,36,37,38,39,40,
    41,42,43,44,45,46,47,48, 49,50,51,-1,-1,-1,-1,-1
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1
};

#define bv(p)   index_64[(p)&0x0ff]

#define B64(b)  ((bv(b[0]) >= 0) && \
                 (bv(b[1]) >= 0) && \
                 (bv(b[2]) >= 0) && \
                 (bv(b[3]) >= 0))

/* decode in place */
static char *
decode_b64(char *buf)
{
  char *p, *p0;
  int val;

  if (!(p0 = p = buf)) return NULL;

  while(*buf) {
    if (*buf == ' ' || *buf == '\t' || *buf == '\n') { buf++; continue;}
    if (!strncmp(buf, "====", 4)) break;
    if (!B64(buf)) break;
    val = (bv(buf[0])<<18) | (bv(buf[1])<<12) | (bv(buf[2])<<6) | (bv(buf[3]));
    p[0] = (val>>16) & 0x0ff;
    p[1] = (val>>8) & 0x0ff;
    p[2] = (val) & 0x0ff;
    if (buf[2] == '=') p += 1;
    else if (buf[3] == '=') p += 2;
    else p += 3;
    buf += 4;
  }
  *p = '\0';
  return p0;
}

/*
 * quote-printable decoding stuff =====================================
 */
/* ==== sbr/fmt_rfc2047.c ==== */
static signed char hexindex[] = {
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
     0, 1, 2, 3, 4, 5, 6, 7,  8, 9,-1,-1,-1,-1,-1,-1,
    -1,10,11,12,13,14,15,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,10,11,12,13,14,15,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1
};

static char *
decode_qp(char *buf)
{
  char *p, *p0;
  int c;

  if (!(p0 = p = buf)) return NULL;

  while (*buf) {
    if ((c = *(buf++)) == '=') {
      if ((hexindex[buf[0]&0x0ff] | hexindex[buf[1]&0x0ff]) >= 0) {
        *(p++) = (hexindex[buf[0]&0x0ff] << 4 ) | hexindex[buf[1]&0x0ff];
        buf += 2;
	continue;
      }
      if (buf[0] == '\n') {
        *p = '\0';  buf += 1;   /* skip new line */
	continue;
      }
      *(p++) = c; /* illegal sequence, but preserve it */
      continue;
    }
    /* normal characters */
    *(p++) = c;
  }
  *p = '\0';
  return p0;
}

/*
 * line-buffered output with charset conversion =======================
 */
static void
putln_with_charset(char *p)
{
  char buf[8192], *ibuf, *obuf;
  size_t ibyte, obyte;

  if (!p) return;

  if (msg_top->ict == ICONV_NULL) {
    fputs(p, stdout);
    return;
  }

  ibuf = p; ibyte = strlen(p);
  obuf = buf; obyte = sizeof(buf);
  (void)iconv(msg_top->ict, NULL, NULL, NULL, NULL);
  while (ibyte > 0) {
    if (iconv(msg_top->ict, &ibuf, &ibyte, &obuf, &obyte) == -1) {
      if (errno == E2BIG) {
	fprintf(stderr, "putln_with_charset(): buffer overflow\n");
	exit(1);
      } else if (errno == EINVAL) {
	ibuf++; ibyte--; /* skip it */
	*obuf++ = '?'; obyte--;
	/* fprintf(stderr, "putln_with_charset(): multibyte sequece error\n"); */
      } else if (errno == EILSEQ) {
	/* skip invalid sequence */
	ibuf++; ibyte--; /* skip it */
	*obuf++ = '?'; obyte--;
	/* fprintf(stderr, "putln_with_charset(): seq error\n"); */
      } else {
	fprintf(stderr, "putln_with_charset(): unknown error#%d\n", errno);
	exit(1);
      }
    }
  }
  *obuf = '\0';
  fputs(buf, stdout);
}

static void
put_line(char *p)
{
  static char buf[4096], *bp = buf; /* holds atmost one line */
  static int cc = sizeof(buf);
  int c;

  if (!p || (*p == '\0')) {
    if (cc == sizeof(buf)) return;
    /* flash buffer */
    if (cc >= 2) {
      bp[0] = '\n';
      bp[1] = '\0';
    } else {
      bp[sizeof(buf)-2] = '\n';
      bp[sizeof(buf)-1] = '\0';
    }	
    putln_with_charset(buf);
    bp = buf;
    cc = sizeof(buf);
    return;
  }

  if (do_b64()) {
    p = decode_b64(p);
  } else if (do_qp()) {
    p = decode_qp(p);
  }
  while ((cc > 1) && (c = *p++)) {
    *bp++ = c;
    cc--;
    if (c == '\n') {
      *bp = '\0';
      if ((sizeof(buf) - cc >= 2) && (bp[-2] == '\r')) {
	bp[-2] = '\n'; bp[-1] = '\0';
      }
      putln_with_charset(buf);
      bp = buf;
      cc = sizeof(buf);
    }
  }
  if (c != '\0') {
    fprintf(stderr, "put_line(): buffer overflow\n");
    exit(1);
  }    
}

/*
 * input buffering ====================================================
 */
#define LNBUF_SZ 100	/* number of buffered lines */
#define LNBUF_LSZ 1024	/* line size in char */
struct lnbuf {
  int next;	/* next position */
  char *lines[LNBUF_SZ];
} lnbuf;

static void
lnbuf_init()
{
   int i;
   char *p;

   lnbuf.next = 0;
   if ((p=(char*)malloc(LNBUF_SZ*LNBUF_LSZ))) {
     for (i=0; i<LNBUF_SZ; i++) {
       lnbuf.lines[i] = p;
       p += LNBUF_LSZ;
     }
   } else {
     fprintf(stderr, "no memory\n");
     exit(1);
   }
}

static char *
lnbuf_first()
{
  lnbuf.next = 0;
  return lnbuf.lines[lnbuf.next];
}

static char *
lnbuf_advance()
{
  lnbuf.next++;
  if (lnbuf.next  >= LNBUF_SZ) {
    fprintf(stderr, "lnbuf_advance(): overflow\n");
    exit(1);
  }
  return lnbuf.lines[lnbuf.next];
}

static char *
lnbuf_field(char *field)
{
  int i, l;
  char *p;

  if (!field || (*field == '\0')) return NULL;

  l = strlen(field);
  for (i=1; i < lnbuf.next; i++) {
    if (!strncmp(lnbuf.lines[i], field, l)) {
      return lnbuf.lines[i];
    }
  }
  return NULL;
}

static void
lnbuf_dump()
{
  int i;

  for (i=0; i <= lnbuf.next; i++) {
    fprintf(stderr, "|%s", lnbuf.lines[i]);
  }
}

/* extract val part from name="val" or name=val */
static char *
extract_val(char *content, char *name)
{
  char val[1024+1], *p;
  int c, i;

  if (!content || (*content == '\0') || !name || (*name == '\0')) return NULL;

  i = 0;
  if ((p = strcasestr(content, name))) {
    p += strlen(name);
    if ((c = *p++) == '"') {
      while ((c = *p++) && (c != '"') && (i < 1024)) val[i++] = c;
    } else {
      do {
	val[i++] = c;
      } while ((c = *p++)
	       && (c != ';') && (c != '\n') && (c != ' ') && (c != '\t')
	       && (i < 1024));
    }
    val[i] = '\0';
    return strdup(val);
  }
  return NULL;
}

static char *
lnbuf_extract_val(char *field, char *name)
{
  int i, l, cc;
  char *val;
  char buf[8192];

  if (!field || (*field == '\0')) return NULL;

  l = strlen(field);
  for (i=1; i < lnbuf.next; i++) {
    if (!strncmp(lnbuf.lines[i], field, l)) {
      buf[0] = '\0';
      do {
	strcat(buf, lnbuf.lines[i]);
	cc = strlen(buf);
	if (buf[cc-1] == '\n') buf[--cc] = '\0';
      }	while ((++i < lnbuf.next) &&
	       ((lnbuf.lines[i][0] == ' ') || (lnbuf.lines[i][0] == '\t')) &&
	       ((cc + strlen(lnbuf.lines[i])) < sizeof(buf)) );
      return extract_val(buf, name);
    }
  }
  return NULL;
}

static char *
trim_word(char *p)
{
  char *p0;

  if (!p || (*p == '\0')) return NULL;
  while (*p == ' ') p++;
  if (*p == '\0') return NULL;
  p0 = p;
  while (*p && (*p != ';') && (*p != ' ') && (*p != '\t') && (*p != '\n')) p++;
  *p = '\0';
  return p0;
}

static int
white_line(char *p)
{
  int c;
  while (p && (c = *(p++))) if (!isspace(c)) return 0;
  return 1;
}

static int
pr_mime_field(char *name)
{
  /* name = '=?csname?[Q|B]?......?=' */
  char csname[128];
  char prefix[128];
  char buf[1024], *p, *q;
  int prefixlen, cc, c;
  int b64;

  if (!name || (*name == '\0')) return 0;

  if (strncmp(name, "=?", 2) ||
      !((q = strcasestr(name, "?B?")) || (q = strcasestr(name, "?Q?"))) ) {
    put_line(" [ ");
    put_line(name);
    put_line(" ]\n");
    return 1;
  }


  strncpy(prefix, name, q-name+3);
  prefix[q-name+3] = '\0';
  prefixlen = strlen(prefix);
  b64 = (prefix[prefixlen - 2] == 'B') || (prefix[prefixlen - 2] == 'b');
  strcpy(csname, prefix+2);
  csname[prefixlen-2-3] = '\0';

  p = buf;
  cc = sizeof(buf);
  while (cc > 1 && *name) {
    if (!strncmp(name, prefix, prefixlen)) {name += prefixlen; continue;}
    if (!strncmp(name , "?=", 2))  {name += 2; continue;}
    if (((c = *name++) == ' ') || (c == '\t')) continue;
    *p++ = c;
    cc--;
  }

  *p = '\0';
  put_line(" [ ");
  set_content(b64?"base64":"quoted-printable", csname);
  put_line(buf);
  set_content(NULL, csname);
  put_line(" ]\n");
  unset_content();

  return 1;
}

/* check boundary */
int
is_boundry(char *p)
{
  if (!msg_top->bndry || !msg_top->bndrylen || !p || (*p == '\0')) return 0;

  if (!strncmp(p, "--", 2) &&
      !strncmp(p+2, msg_top->bndry, msg_top->bndrylen)) {
    if (white_line(p+2+msg_top->bndrylen))
      return 1;
    if (!strncmp(p+2+msg_top->bndrylen, "--", 2) &&
	white_line(p+2+msg_top->bndrylen+2))
      return 2; /* last boundary */
  }
  return 0;
}

/* recognize following Content-XXX */
#define	CT_TYPE		"Content-Type:"
#define	CT_TRENCODE	"Content-Transfer-Encoding:"
#define	CT_DISPO	"Content-Disposition:"

#define CT_TYPE_TEXT	"text/plain"
#define CT_TYPE_APPLI	"application/"
#define CT_TYPE_MULTI	"multipart/"

int
main(int argc, char *argv[])
{
  int i;
  char *l, *ln, *fld;
  char *arg_content, *arg_tr_encode;
  char *bndry, *csname, *tr_encode;
  int one_shot = 0;
  int is_digest = 0;

  arg_content = arg_tr_encode = NULL;

  if (((l=getenv("LANG")) && (*l != '\0')) ||
      ((l=getenv("LC_ALL")) && (*l != '\0')) ||
      ((l=getenv("LC_CTYPE")) && (*l != '\0'))) {
    setlocale(LC_ALL, "");
    out_charset = iconv_canonicalize(nl_langinfo(CODESET));
  }

  lnbuf_init();

  /* process ":=HDRT=:....:=HDRE=:..." */
  if ((argc == 2)
      && !strncmp(argv[1], HDR_TYPE_STR, strlen(HDR_TYPE_STR))
      && ((l=strstr(argv[1], HDR_TRENCODE_STR))) ) {
    arg_content = argv[1] + strlen(HDR_TYPE_STR);
    *l = '\0';
    arg_tr_encode = l + strlen(HDR_TRENCODE_STR) ;
    if (*arg_content == '\0') arg_content = NULL;
  } else {
  try_again:
    /* no arg --> guess */
    l = lnbuf_first();
    /* skip to the line mimics to a boundary line */
    while ((ln=fgets(l, LNBUF_LSZ, stdin)) == l) {
      if (strncmp(l, "--", 2)) continue;
      if (strncmp(l+strlen(l)-2, "--", 2)) break;
    }
    if (!ln) return 0;
    push_content(NULL, NULL, strdup(l+2));
    while ((l = lnbuf_advance()) && ((ln=fgets(l, LNBUF_LSZ, stdin)) == l)) {
      if (!strncasecmp(l, "Content-", strlen("Content-"))) continue;
      if ((i=white_line(l))) break;
    }
    if (!ln) return 0;
    /* lnbuf_dump(); */
    if (!i) {pop_content(); goto try_again;}
    /* extract text/plain only */
    if (!(fld = lnbuf_field(CT_TYPE)) || !strcasestr(fld, CT_TYPE_TEXT)) {
      pop_content(); goto try_again;
    }
    one_shot = 1;
    goto type_text;
  }

  if ((arg_content == NULL) || (*arg_content == '\0') ||
      strcasestr(arg_content, CT_TYPE_TEXT) ) {
    /* simple text -- just check iso-2022-jp */
    csname = NULL;
    push_content(arg_tr_encode,
		 (csname=extract_val(arg_content, "charset="))?csname:"guess",
		 NULL);
    if (csname) free(csname); /* XXX */

    l = lnbuf_first();
    while (fgets(l, LNBUF_LSZ, stdin) == l) {
      put_line(l);
    }
    put_line(NULL); /* flash buffer */
    return 0;
  }

  /* multipart/{mixed,alternative} */
  if (strncasecmp(arg_content, CT_TYPE_MULTI, strlen(CT_TYPE_MULTI)) ||
      !(bndry = extract_val(arg_content, "boundary=")) ) {
    fprintf(stderr, "no boundary specified\n");
    exit(1);
  }
  push_content(NULL, NULL, bndry);
  is_digest = !strncasecmp(arg_content,
			   CT_TYPE_MULTI "digest", strlen(CT_TYPE_MULTI "digest"));

 loop0:
  l = lnbuf_first();
  if (fgets(l, LNBUF_LSZ, stdin) != l) exit(1);

 loop:
  /* skip to boundary line -- `l' is lnbuf_first() and has boundary line */
  do {
    switch (is_boundry(l)) {
    case 1:
      if (is_digest) {
	while (fgets(l, LNBUF_LSZ, stdin) == l) {
	  if (!white_line(l)) break;
	}
      }
      goto check_content;
    case 2:
      pop_content();
      if (content_empty()) return 0;
    }
  } while ((ln=fgets(l, LNBUF_LSZ, stdin)) == l);
  if (!ln) exit(1);
  
 check_content:
  /* collect Content- */
  i = 0;
  while ((l = lnbuf_advance()) && ((ln=fgets(l, LNBUF_LSZ, stdin)) == l)) {
    if ((i=white_line(l))) break;
  }
  if (!i || !(fld = lnbuf_field(CT_TYPE)) || !ln ) {
    fprintf(stderr, "mime format error?\n");
    lnbuf_dump();
    exit(1);
  }
  if (strcasestr(fld, CT_TYPE_MULTI)) {
    if ((bndry = lnbuf_extract_val(CT_TYPE, "boundary="))) {
      push_content(NULL, NULL, bndry);
      goto loop0;
    }
    fprintf(stderr, "mime format error multipart/?\n");
    lnbuf_dump();
    exit(1);
  }

 type_text:
  if (strcasestr(fld, CT_TYPE_TEXT)) {
    if ((tr_encode = lnbuf_field(CT_TRENCODE)))
      tr_encode = trim_word(tr_encode + strlen(CT_TRENCODE));
    else
      tr_encode = NULL;
    csname = lnbuf_extract_val(CT_TYPE, "charset=");
    set_content(tr_encode, csname);
    if (csname) free(csname); /* XXX */
    l = lnbuf_first();
    while ((ln=fgets(l, LNBUF_LSZ, stdin)) == l) {
      if (is_boundry(l)) break;
      put_line(l);
    }
    if (!ln) exit(1);
    put_line(NULL);
    unset_content();
    if (one_shot) return 0;
    goto loop;
  }
  
  if (strcasestr(fld, CT_TYPE_APPLI)) {
    char *name;
    if (!(name = lnbuf_extract_val(CT_TYPE, "name="))) {
      name = lnbuf_extract_val(CT_DISPO, "filename=");
    }
    if (!pr_mime_field(name))
      put_line(" [ application content ]\n");
    if (name) free(name); /* XXX */
    goto loop0;
  }

  /* other things */
  goto loop0;

  return 0;
}
