#include <stdint.h>

#define WORD int32_t
#define WORD_BITS 32
#define DWORD int64_t
#define BOOL int

const WORD MAX_WORD = 4294967295; // 2^32 - 1
const DWORD WORD_BIT_MASK = 0xFFFFFFFF; // set to 1 for all and only bits in the first word

// The heap size. This may need to be adjusted on a per-program basis.
const WORD HEAP_SIZE = 1024 * 16;

// We represent program failure by non-termination.
void diverge();


/*****************************************************************************
 * Memory management
 *****************************************************************************/

// The first thing is a memory manager. This RTS uses a never-freed memory
// management model, meaning there is no manual memory manager and no GC;
// memory, once allocated, is never freed. This should work for our purposes
// where programs are expected to have short lifetimes.

// Technically, the heap is stored on the stack, and the main() function
// must set these global variables to point respectively to the end of the
// heap (heap_end) and the next free byte in the heap (heap_free);

extern void *heap_end;
extern void *heap_free;

// Allocates the given number of bytes of memory and returns a pointer to the
// beginning of the allocated memory.
void *alloc(WORD bytes);


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

// Returns a fresh copy of the given natural number.
struct Natural *copy_nat(struct Natural *a);

// Adds a to b, destructively updating b.
void add_nat(struct Natural *a, struct Natural *b);

// Returns true if a is equal to b.
BOOL eq_nat(struct Natural *a, struct Natural *b);

// Returns true if a <= b.
BOOL leq_nat(struct Natural *a, struct Natural *b);

// Subtracts natural number a from integer b, destructively updating b.
void subtract_nat(struct Natural *a, struct Integer *b);

// Multiplies b by a, destructively updating b.
void mul_nat(struct Natural *a, struct Natural *b);

// Adds a to b, destructively updating b.
void add_int(struct Integer *a, struct Integer *b);

// Subtracts a from b, destructively updating b.
void subtract_int(struct Integer *a, struct Integer *b);

// Multiplies b by a, destructively updating b.
void mul_int(struct Integer *a, struct Integer *b);


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
