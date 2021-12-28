// uplc2c runtime system.
// This C code is to be included in the uplc2c compiler output for each program.
// This RTS is designed for compact code more so than performance or efficiency.

// We represent program failure by non-termination.
void diverge() {
  while (true) {}
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

void *malloc(int bytes) {
  if (heap_free + bytes < heap_end) {
    void *new_mem = heap_free;
    heap_free += (void *)bytes;
    return new_mem;
  } else {
    diverge();
  }
}


/*****************************************************************************
 * Integers
 *****************************************************************************/

// Integers are unlimited in size. We represent them as linked lists of words,
// least significant word first, with the sign being stored in the most
// significant word.

struct Integer {
  int less_significant;
  BigInteger *more_significant; // null if this is the most significant word.
}


/*****************************************************************************
 * NFData
 *****************************************************************************/

// The next thing is the representation of NFData, which is in other
// words data (or equivalently, code) which is in beta normal form.

enum NFDataType {
    Function
  , Integer
  , Unit
  , Bool
  , Thunk // (result of Delay)
  , Text
  , ByteString
  // , Data // TODO
};

// A function is represented by a closure, which is a function pointer together with
// some data on the heap which it depends on. At this level of abstraction we
// do not distinguish between builtin and user-defined functions.
struct Closure {
  NFData (*function)(void *, NFData);
  void *data; // may be null if the function does not dereference the pointer
};

// A thunk is effectively a closure over a function with no arguments.
struct Thunk {
  NFData (*function)(void *);
  void *data;
};

// This struct represents a Text or a ByteString depending on the type tag.
struct ByteString {
  int length;
  char *bytes;
}

union NFDataValue {
  Closure closure;
  Integer integer;
  bool boolean;
  int unit; // the value stored here is meaningless / does not affect execution
  Thunk thunk;
  ByteString byteString;
};

struct NFData {
  NFDataType type;
  NFDataValue value;
};
