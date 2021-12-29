// uplc2c runtime system.
// This C code is to be included in the uplc2c compiler output for each program.
// This RTS is designed for compact code more so than performance or efficiency.

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
const int HEAP_SIZE = 1024 * 16;

// Technically, the heap is stored on the stack, and the main() function
// must set these global variables to point respectively to the end of the
// heap (heap_end) and the next free byte in the heap (heap_free);
void *heap_end;
void *heap_free;

void *alloc(int bytes) {
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
  int less_significant; // must be non-negative
  struct Natural *more_significant; // null if this is the most significant word.
};


struct Integer {
  int sign;
  Natural nat;
};


// Adds a to b, destructively updating b.
void add_nat (Natural *a, Natural *b) {
  int a_ls = a->less_significant;
  int b_ls = b->less_significant;
  int carry = 0;
  int d = a_ls + b_ls;
  if (d < 0) {
    carry = 1;
    d = (a_ls - MAX_INT) + b_ls;
  }
  b->less_significant = d;
  struct Natural *a_ms = a->more_significant;
  struct Natural *b_ms = b->more_significant;
  if (a_ms || b_ms || carry) {
    if (!b_ms) {
      b_ms = (Natural *)alloc(sizeof(Natural));
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
int eq_nat (Natural *a, Natural *b) {
  if (a->less_significant == b->less_significant) {
    Natural *a_ms = a->more_significant;
    Natural *b_ms = b->more_significant;
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
int leq_nat (Natural *a, Natural *b) {
  if (eq_nat(a, b)) {
    return 1;
  } else {
    Natural *a_ms = a->more_significant;
    Natural *b_ms = b->more_significant;
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
void subtract_nat (Natural *a, Integer *b) {
  if (b->sign == -1) {
    add_nat(a, b->nat);
  } else if (leq_nat(a, b->nat)) {
    // b >= 0 and a <= b, so b - a >= 0
    Natural *a_ms = a->more_significant;
    Natural *b_ms = b->nat->more_significant;
    int a_ls = a->less_significant;
    int b_ls = b->less_significant;
    if (!(a_ms || b_ms)) {
      if (a_ls <= b_ls) {
        b->less_significant = b_ls - a_ls;
      } else {
        b->sign = -1;
	b->less_significant = a_ls - b_ls;
      }
    } else if (a_ms && b_ms) {
      int carry = 0;
      if (a_ls <= b_ls) {
        b->less_significant -= a_ls;
      } else {
	// a_ls > b_ls
        carry = 1;
	b->less_significant = (b_ls - MAX_INT) + a_ls;
      }
      if (carry) {
        Natural c;
        c.less_significant = carry;
        c.more_significant = 0;
	subtract_nat(&c, b_ms); // type error, int vs nat
      }
      subtract_nat(a_ms, b_ms); // type error, int vs nat
    } else {
      // a_ms || b_ms but !(a_ms && b_ms), but a <= b, so, !a_ms && b_ms
      int a_ls = a->less_significant;
      int b_ls = b->less_significant;
      if (a_ls <= b_ls) {
        b->less_significant -= a_ls;
      } else {
	struct Natural one;
	one.less_significant = 1;
	one.more_significant = 0;
	struct Natural zero;
	zero.less_significant = 0;
	zero.more_significant = 0;
	subtract_nat(one, b_ms); // type error, int vs nat
	if (eq_nat(b_ms, zero)) {
          b->more_significant = 0;
	}
	b->less_significant = (b - a) + MAX_INT;
      }
    }
  } else {
    // b >= 0 and a > b, so b - a < 0
    Integer c;
    c.sign = -1;
    c.nat = a;
    subtract_nat(b->nat, &c);
    b->nat = c.nat;
  }
}

// Adds a to b, destructively updating b.


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
