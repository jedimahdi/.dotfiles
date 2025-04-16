$@	Target name
$<	First prerequisite
$^	All prerequisites
$*	The stem (matched by %)
$(@D)	Directory of the target
$(@F)	Filename of the target
patsubst	Replace patterns in filenames
subst	Replace text in strings
wildcard	Find files using a pattern
notdir	Extract filename from a path
dir	Extract directory from a path
basename	Remove file extension
suffix	Get file extension


Recursive .c file detection	$(wildcard src/**/*.c)
Keeping .o files organized	$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
= is lazy


```make
SRC_DIR := src
OBJ_DIR := obj
CC := gcc
CFLAGS := -Wall -Wextra -g
LDFLAGS := -lm  # Example: linking with the math library

# Find all .c files recursively in src/
SOURCES := $(wildcard $(SRC_DIR)/**/*.c)

# Convert src/foo/bar.c → obj/foo/bar.o
OBJECTS := $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR)/%.o, $(SOURCES))

# Final executable
TARGET := myprogram

all: $(TARGET)

# Link object files into the final executable
$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Compile any .c file to .o while keeping directory structure
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(@D)  # Ensure directory exists
	$(CC) $(CFLAGS) -c $< -o $@

# Cleanup
clean:
	rm -rf $(OBJ_DIR) $(TARGET)
```

