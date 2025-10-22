SRC_DIR := bootstrap
BUILD_DIR := build
BIN_DIR := bin

TARGET := $(BIN_DIR)/bootstrap

CC := gcc
CFLAGS := -Wall -Wextra -Wno-unused -I./bootstrap -g -std=c99 -pedantic -MMD -MP

SRCS := $(wildcard $(SRC_DIR)/*.c)
OBJS := $(SRCS:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)
DEPS := $(OBJS:.o=.d)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR) $(TARGET)

.PHONY: all clean

-include $(DEPS)
