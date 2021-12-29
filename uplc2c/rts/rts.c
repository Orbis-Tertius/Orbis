// uplc2c runtime system.
// This C code is to be included in the uplc2c compiler output for each program.
// This RTS is designed for compact code more so than performance or efficiency.


#include <stdint.h>

#define WORD int32_t
#define DWORD int64_t
#define BOOL int

const extern WORD MAX_WORD;


// We represent program failure by non-termination.
void diverge() {
  while (1) {}
}


/*****************************************************************************
 * Memory management
 *****************************************************************************/

// The first thing is a memory manager. This RTS uses a never-freed memory
// management model, meaning there is no manual memory manager and no GC;
// memory, once allocated, is never freed. This should work for our purposes
// where programs are expected to have short lifetimes.

// The heap size. This may need to be adjusted on a per-program basis.
const WORD HEAP_SIZE = 1024 * 16;

// Technically, the heap is stored on the stack, and the main() function
// must set these global variables to point respectively to the end of the
// heap (heap_end) and the next free byte in the heap (heap_free);
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

// Integers are unlimited in size. We store the sign of an integer
// in the type tag. The rest of the integer is two words to represent a natural.
// Natural numbers are represented as linked lists of words,
// least significant word first.

struct Natural {
  WORD less_significant; // must be non-negative
  struct Natural *more_significant; // null if this is the most significant word.
};


struct Integer {
  WORD sign;
  struct Natural *nat;
};


// Adds a to b, destructively updating b.
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

// Returns true if a is equal to b.
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

// Returns true if a <= b.
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

// Subtracts natural number a from integer b, destructively updating b.
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

// Adds a to b, destructively updating b.
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


// Subtracts a from b, destructively updating b.
void subtract_int(struct Integer *a, struct Integer *b) {
  struct Integer c;
  c.sign = -a->sign;
  c.nat = a->nat;
  add_int(&c, b);
}


/*****************************************************************************
 * NFData
 *****************************************************************************/

// The next thing is the representation of NFData, which is in other
// words data (or equivalently, code) which is in beta normal form.

enum NFDataType {
    FunctionType
  , IntegerPositive
  , IntegerNegative
  , UnitType
  , BoolType
  , ThunkType // (result of Delay)
  , TextType
  , ByteStringType
  // , Data // TODO
};

// The third argument is a pointer to where to put the output. Is actually an
// (NFData *) but that would make for cyclic definitions. Similarly, the second
// argument is an (NFData *) but is represented as (void *) to avoid a cycle.
typedef void (*Function) (void *context, void *input, void *output);

// A function is represented by a closure, which is a function pointer together with
// some data on the heap which it depends on. At this level of abstraction we
// do not distinguish between builtin and user-defined functions.
struct Closure {
  Function apply;
  void *data; // may be null if the function does not dereference the pointer
};

// The second argument is a point to where to put the output. Is actually an
// (NFData *) but that would make for cyclic definitions.
typedef void (*Computation) (void *context, void *output);

// A thunk is effectively a closure over a function with no arguments.
struct Thunk {
  Computation run;
  void *data;
};

// This struct represents a Text or a ByteString depending on the type tag.
struct ByteString {
  int length;
  char *bytes;
};

union NFDataValue {
  struct Closure fn;
  struct Natural nat;
  int boolean;
  int unit; // the value stored here is meaningless / does not affect execution
  struct Thunk thunk;
  struct ByteString byteString;
};

struct {
  enum NFDataType type;
  union NFDataValue value;
} NFData;
