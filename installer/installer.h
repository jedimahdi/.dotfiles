#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NOB_ASSERT assert
#define NOB_REALLOC realloc
#define NOB_FREE free

typedef enum {
  NOB_INFO,
  NOB_WARNING,
  NOB_ERROR,
} Nob_Log_Level;

void nob_log(Nob_Log_Level level, const char *fmt, ...);

bool nob_mkdir_if_not_exists(const char *path);

#define nob_return_defer(value)                                                \
  do {                                                                         \
    result = (value);                                                          \
    goto defer;                                                                \
  } while (0)

// Initial capacity of a dynamic array
#define NOB_DA_INIT_CAP 256

// Append an item to a dynamic array
#define nob_da_append(da, item)                                                \
  do {                                                                         \
    if ((da)->count >= (da)->capacity) {                                       \
      (da)->capacity =                                                         \
          (da)->capacity == 0 ? NOB_DA_INIT_CAP : (da)->capacity * 2;          \
      (da)->items =                                                            \
          NOB_REALLOC((da)->items, (da)->capacity * sizeof(*(da)->items));     \
      NOB_ASSERT((da)->items != NULL && "Buy more RAM lol");                   \
    }                                                                          \
                                                                               \
    (da)->items[(da)->count++] = (item);                                       \
  } while (0)

#define nob_da_free(da) NOB_FREE((da).items)

// Append several items to a dynamic array
#define nob_da_append_many(da, new_items, new_items_count)                     \
  do {                                                                         \
    if ((da)->count + (new_items_count) > (da)->capacity) {                    \
      if ((da)->capacity == 0) {                                               \
        (da)->capacity = NOB_DA_INIT_CAP;                                      \
      }                                                                        \
      while ((da)->count + (new_items_count) > (da)->capacity) {               \
        (da)->capacity *= 2;                                                   \
      }                                                                        \
      (da)->items =                                                            \
          NOB_REALLOC((da)->items, (da)->capacity * sizeof(*(da)->items));     \
      NOB_ASSERT((da)->items != NULL && "Buy more RAM lol");                   \
    }                                                                          \
    memcpy((da)->items + (da)->count, (new_items),                             \
           (new_items_count) * sizeof(*(da)->items));                          \
    (da)->count += (new_items_count);                                          \
  } while (0)

typedef struct {
  char *items;
  size_t count;
  size_t capacity;
} Nob_String_Builder;

// Append a sized buffer to a string builder
#define nob_sb_append_buf(sb, buf, size) nob_da_append_many(sb, buf, size)

// Append a NULL-terminated string to a string builder
#define nob_sb_append_cstr(sb, cstr)                                           \
  do {                                                                         \
    const char *s = (cstr);                                                    \
    size_t n = strlen(s);                                                      \
    nob_da_append_many(sb, s, n);                                              \
  } while (0)

// Append a single NULL character at the end of a string builder. So then you
// can use it a NULL-terminated C string
#define nob_sb_append_null(sb) nob_da_append_many(sb, "", 1)

// Free the memory allocated by a string builder
#define nob_sb_free(sb) NOB_FREE((sb).items)

typedef int Nob_Proc;
#define NOB_INVALID_PROC (-1)

typedef struct {
  Nob_Proc *items;
  size_t count;
  size_t capacity;
} Nob_Procs;

bool nob_proc_wait(Nob_Proc proc);

// A command - the main workhorse of Nob. Nob is all about building commands an
// running them
typedef struct {
  const char **items;
  size_t count;
  size_t capacity;
} Nob_Cmd;

// Render a string representation of a command into a string builder. Keep in
// mind the the string builder is not NULL-terminated by default. Use
// nob_sb_append_null if you plan to use it as a C string.
void nob_cmd_render(Nob_Cmd cmd, Nob_String_Builder *render);

#define nob_cmd_append(cmd, ...)                                               \
  nob_da_append_many(                                                          \
      cmd, ((const char *[]){__VA_ARGS__}),                                    \
      (sizeof((const char *[]){__VA_ARGS__}) / sizeof(const char *)))

// Free all the memory allocated by command arguments
#define nob_cmd_free(cmd) NOB_FREE(cmd.items)

// Run command asynchronously
Nob_Proc nob_cmd_run_async(Nob_Cmd cmd);

// Run command synchronously
bool nob_cmd_run_sync(Nob_Cmd cmd);
