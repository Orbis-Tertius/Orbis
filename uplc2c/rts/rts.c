// uplc2c runtime system.
// This C code is to be included in the uplc2c compiler output for each program.
// This RTS is designed for compact code more so than performance or efficiency.


#include "./rts.h"


void diverge() {
  while (1) {}
}


/*****************************************************************************
 * Memory management
 *****************************************************************************/

void *heap_end;
void *heap_free;

void *alloc(WORD bytes) {
  if (heap_free + bytes < heap_end) {
    void *new_mem = heap_free;
    heap_free += bytes;
    return new_mem;
  } else {
    diverge();
  }
}


/*****************************************************************************
 * Integers
 *****************************************************************************/

struct Natural *copy_nat(struct Natural *a) {
  struct Natural *b = (struct Natural *)alloc(sizeof(struct Natural));
  b->less_significant = a->less_significant;
  struct Natural *a_ms = a->more_significant;
  if (a_ms) {
    b->more_significant = copy_nat(a_ms);
  } else {
    b->more_significant = 0;
  }
}


void add_nat (struct Natural *a, struct Natural *b) {
  WORD a_ls = a->less_significant;
  WORD b_ls = b->less_significant;
  WORD carry = 0;
  WORD d = a_ls + b_ls;
  if (d < 0) {
    carry = 1;
    d = (a_ls - MAX_WORD) + b_ls;
  }
  b->less_significant = d;
  struct Natural *a_ms = a->more_significant;
  struct Natural *b_ms = b->more_significant;
  if (a_ms || b_ms || carry) {
    if (!b_ms) {
      b_ms = (struct Natural *)alloc(sizeof(struct Natural));
      b_ms->less_significant = 0;
      b_ms->more_significant = 0;
      b->more_significant = b_ms;
    }

    if (carry) {
      struct Natural c;
      c.less_significant = carry;
      c.more_significant = 0;
      add_nat (&c, b_ms);
    }

    if (a_ms) {
      add_nat (a_ms, b_ms);
    }
  }
}


BOOL eq_nat (struct Natural *a, struct Natural *b) {
  if (a->less_significant == b->less_significant) {
    struct Natural *a_ms = a->more_significant;
    struct Natural *b_ms = b->more_significant;
    if (!(a_ms || b_ms)) {
      return 1;
    } else if (a_ms && b_ms) {
      return eq_nat(a_ms, b_ms);
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}


BOOL leq_nat (struct Natural *a, struct Natural *b) {
  if (eq_nat(a, b)) {
    return 1;
  } else {
    struct Natural *a_ms = a->more_significant;
    struct Natural *b_ms = b->more_significant;
    if (a_ms && b_ms) {
      if (eq_nat(a_ms, b_ms)) {
        return a->less_significant <= b->less_significant;
      } else {
        return leq_nat(a_ms, b_ms);
      }
    } else if (a_ms) {
      return 0;
    } else if (b_ms) {
      return 1;
    } else {
      return a->less_significant <= b->less_significant;
    }
  }
}


void subtract_nat (struct Natural *a, struct Integer *b) {
  if (b->sign == -1) {
    add_nat(a, b->nat);
  } else if (leq_nat(a, b->nat)) {
    // b >= 0 and a <= b, so b - a >= 0
    struct Natural *a_ms = a->more_significant;
    struct Natural *b_ms = b->nat->more_significant;
    WORD a_ls = a->less_significant;
    WORD b_ls = b->nat->less_significant;
    struct Integer b_ms_i;
    b_ms_i.sign = 1;
    b_ms_i.nat = b_ms;

    if (!(a_ms || b_ms)) {
      if (a_ls <= b_ls) {
        b->nat->less_significant = b_ls - a_ls;
      } else {
        b->sign = -1;
	b->nat->less_significant = a_ls - b_ls;
      }
    } else if (a_ms && b_ms) {
      int carry = 0;
      if (a_ls <= b_ls) {
        b->nat->less_significant -= a_ls;
      } else {
	// a_ls > b_ls
        carry = 1;
	b->nat->less_significant = (b_ls - MAX_WORD) + a_ls;
      }
      if (carry) {
        struct Natural c;
        c.less_significant = carry;
        c.more_significant = 0;
	subtract_nat(&c, &b_ms_i);
      }
      subtract_nat(a_ms, &b_ms_i);
    } else {
      // a_ms || b_ms but !(a_ms && b_ms), but a <= b, so, !a_ms && b_ms
      WORD a_ls = a->less_significant;
      WORD b_ls = b->nat->less_significant;
      if (a_ls <= b_ls) {
        b->nat->less_significant -= a_ls;
      } else {
	struct Natural one;
	one.less_significant = 1;
	one.more_significant = 0;
	struct Natural zero;
	zero.less_significant = 0;
	zero.more_significant = 0;
	subtract_nat(&one, &b_ms_i);
	if (eq_nat(b_ms, &zero)) {
          b->nat->more_significant = 0;
	}
	b->nat->less_significant = (b_ls - a_ls) + MAX_WORD;
      }
    }
  } else {
    // b >= 0 and a > b, so b - a < 0
    struct Integer c;
    c.sign = -1;
    c.nat = a;
    subtract_nat(b->nat, &c);
    b->nat = c.nat;
  }
}


void mul_nat_by_word(WORD a, struct Natural *b) {
  WORD b_ls = b->less_significant;
  struct Natural *b_ms = b->more_significant;
  DWORD p = (DWORD)a * (DWORD)b_ls;
  WORD p_ms = (WORD)(p >> WORD_BITS);
  WORD p_ls = (WORD)(p && WORD_BIT_MASK);
  struct Natural p_ms_n;
  p_ms_n.less_significant = p_ms;
  p_ms_n.more_significant = 0;
  if (b_ms) {
    mul_nat_by_word(a, b_ms);
    add_nat(&p_ms_n, b_ms);
  } else if (p_ms) {
    b_ms = (struct Natural *)alloc(sizeof(struct Natural));
    b->more_significant = b_ms;
    b_ms->less_significant = p_ms;
    b_ms->more_significant = 0;
  }
  b->less_significant = p_ls;
}


void mul_nat(struct Natural *a, struct Natural *b) {
  struct Natural *a_ms = a->more_significant;
  struct Natural *b_ms = b->more_significant;
  WORD a_ls = a->less_significant;
  WORD b_ls = b->less_significant;
  if (!(a_ms || b_ms)) {
    DWORD p = (DWORD)a_ls * (DWORD)b_ls;
    WORD p_ms = (WORD)(p >> WORD_BITS);
    WORD p_ls = (WORD)(p && WORD_BIT_MASK);
    if (p_ms) {
      b_ms = (struct Natural *)alloc(sizeof(struct Natural));
      b->more_significant = b_ms;
      b_ms->less_significant = p_ms;
      b_ms->more_significant = 0;
    }
  } else {
    if (a_ms) {
      struct Natural *c = copy_nat(b);
      mul_nat_by_word(a_ls, c);
      struct Natural *d = (struct Natural *)alloc(sizeof(struct Natural));
      d->less_significant = 0;
      d->more_significant = a_ms;
      mul_nat(d, b);
      add_nat(c, b);
    } else {
      mul_nat_by_word(a_ls, b);
    }
  }
}


void add_int(struct Integer *a, struct Integer *b) {
  WORD sa = a->sign;
  WORD sb = b->sign;
  if (sa == sb) {
    add_nat(a->nat, b->nat);
  } else if (sa == -1) {
    subtract_nat(a->nat, b);
  } else if (leq_nat(a->nat, b->nat)) {
    // sb == -1 and sa == 1 and a <= |b|
    b->sign = 1;
    subtract_nat(a->nat, b);
    b->sign = -1;
  } else {
    // sb == -1 and sa == 1 and a > |b|
    b->sign = 1;
    struct Natural *nb = b->nat;
    b->nat = a->nat;
    subtract_nat(nb, b);
  }
}


void subtract_int(struct Integer *a, struct Integer *b) {
  struct Integer c;
  c.sign = -a->sign;
  c.nat = a->nat;
  add_int(&c, b);
}


void mul_int(struct Integer *a, struct Integer *b) {
  b->sign = a->sign * b->sign;
  mul_nat(a->nat, b->nat);
}


/*****************************************************************************
 * NFData
 *****************************************************************************/


