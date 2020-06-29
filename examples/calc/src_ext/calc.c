#include <stdio.h>
#include <stdlib.h>

void eval(void);

void calc_entry(void) {
  printf("calc\n");
  eval();
}

typedef struct {
  char *buffer;
  int offset;
} Parser;

int parse_expr(Parser *p, int *result);

char get_char_at(Parser *p, int index) {
  return p->buffer[index];
}

char get_char(Parser *p) {
  return get_char_at(p, p->offset);
}

char step(Parser *p) {
  p->offset++;
}

void skip_space(Parser *p) {
  char ch = get_char(p);
  while (ch == ' ')  {
    step(p);
    ch = get_char(p);
  }
}

int parse_number(Parser *p, int* result) {
  int begin = p->offset;
  int end = begin;

  for(;;) {
    char ch = get_char(p);
    if (!(ch >= '0' && ch <= '9')) {
      end = p->offset;
      break;
    }

    step(p);
  }

  if (begin == end) {
    return 1;
  }

  int num = 0;
  int mul = 1;

  while(end > begin) {
    --end;

    char ch = p->buffer[end];
    num += (ch - '0') * mul;
    mul *= 10;
  }

  *result = num;

  return 0;
}

int parse_primitive(Parser *p, int *result) {
  int offset = p->offset;
  skip_space(p);

  char ch = get_char(p);
  if (ch == '(') {
    step(p);

    if (parse_expr(p, result) != 0) {
      p->offset = offset;
      return 1;
    }

    skip_space(p);
    char ch = get_char(p);
    if (ch != ')') {
      p->offset = offset;
      return 1;
    }
    step(p);

    return 0;
  }

  return parse_number(p, result);
}

int parse_mul(Parser *p, int *result) {
  int offset = p->offset;
  skip_space(p);

  int calced = 0;
  if (parse_primitive(p, &calced) != 0) {
    p->offset = offset;
    return 1;
  }
  offset = p->offset;

  skip_space(p);
  char ch = get_char(p);
  while(ch == '*' || ch == '/') {
    step(p);

    skip_space(p);
    int num;
    if (parse_primitive(p, &num) != 0) {
      break;
    }
    offset = p->offset;

    if (ch == '*') calced *= num;
    else if (ch == '/') calced /= num;

    skip_space(p);
    ch = get_char(p);
  }

  p->offset = offset;

  *result = calced;

  return 0;
}

int parse_add(Parser *p, int *result) {
  int offset = p->offset;
  skip_space(p);

  int calced = 0;
  if (parse_mul(p, &calced) != 0) {
    p->offset = offset;
    return 1;
  }
  offset = p->offset;

  skip_space(p);
  char ch = get_char(p);
  while(ch == '+' || ch == '-') {
    step(p);

    skip_space(p);
    int num;
    if (parse_mul(p, &num) != 0) {
      break;
    }
    offset = p->offset;

    if (ch == '+') calced += num;
    else if (ch == '-') calced -= num;

    skip_space(p);
    ch = get_char(p);
  }

  p->offset = offset;

  *result = calced;

  return 0;
}

int parse_expr(Parser *p, int *result) {
  return parse_add(p, result);
}

int parse_with_action(Parser *p, int *result) {
  return parse_expr(p, result);
}

void eval(void) {
  char *line = NULL;
  size_t n = 0;

  size_t len = getline(&line, &n, stdin);
  if (len == -1) {
    printf("Failed to getline\n");
    goto finish;
  }
  printf("len: %d; line: %s", len, line);

  Parser p;
  p.buffer = line;
  p.offset = 0;

  int result;
  if (parse_with_action(&p, &result) != 0) {
    printf("Failed to parse\n");
    goto finish;
  }

  printf("= %d\n", result);

 finish:
    free(line);
}
