# Directories:
OBJ_DIR=obj
BIN_DIR=bin
OUT_DIR=out
SRC_DIR=src
TEST_DIR=$(OUT_DIR)/test

# Don't worry about modification times on output directories:
MAKEFLAGS+=--assume-old=$(OBJ_DIR)
MAKEFLAGS+=--assume-old=$(BIN_DIR)
MAKEFLAGS+=--assume-old=$(OUT_DIR)
MAKEFLAGS+=--assume-old=$(TEST_DIR)

# Compiler & Flags:
CC=gcc
DEBUG_FLAGS=-g -O1 -DDEBUG
#PROFILE_FLAGS=-pg
PROFILE_FLAGS=
OPT_FLAGS=-O3
INCLUDE_FLAGS=-I$(SRC_DIR)
CFLAGS=-c -Wall -ffast-math $(INCLUDE_FLAGS) $(DEBUG_FLAGS) $(PROFILE_FLAGS)

#LIBS_MYLIB=
LIBS=

LFLAGS=-lm $(LIBS) $(PROFILE_FLAGS)

# Objects:
CORE_OBJECTS=$(OBJ_DIR)/story.o \
             $(OBJ_DIR)/vector.o \
             $(OBJ_DIR)/list.o \
             $(OBJ_DIR)/queue.o \
             $(OBJ_DIR)/map.o \
             $(OBJ_DIR)/bitmap.o

MAIN_OBJECTS=$(OBJ_DIR)/main.o

UNIT_TEST_OBJECTS=$(OBJ_DIR)/test_suite.o \
						      $(OBJ_DIR)/unit_tests.o

# The default goal:
.DEFAULT_GOAL := main

.PHONY: all clean main unit_tests

all: main unit_tests
clean:
	rm -rf $(OBJ_DIR)
	rm -rf $(BIN_DIR)
	rm -rf $(OUT_DIR)
main: $(BIN_DIR)/dunyazad
unit_tests: $(BIN_DIR)/unit_tests

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

$(TEST_DIR):
	mkdir -p $(TEST_DIR)

$(SRC_DIR)/*.c: ;
$(SRC_DIR)/*/*.c: ;
$(SRC_DIR)/*.h: ;
$(SRC_DIR)/*/*.h: ;

# Automatically determine the dependencies between source/header files:
$(OBJ_DIR)/obj.d: \
$(SRC_DIR)/*.c \
$(SRC_DIR)/*/*.c \
$(SRC_DIR)/*.h \
$(SRC_DIR)/*/*.h  \
$(OBJ_DIR)
	$(CC) -MM $(INCLUDE_FLAGS) $(SRC_DIR)/*.c $(SRC_DIR)/*/*.c |\
		sed "s/^\([^ ]\)/$(OBJ_DIR)\/\1/" >\
		$(OBJ_DIR)/obj.d

Makefile: ;

# Make sure that we remake the dependency file before building (unnecessary?):
#Makefile: $(OBJ_DIR)/obj.d

include $(OBJ_DIR)/obj.d

# How to build things based on their dependencies as specified in obj.d:
$(OBJ_DIR)/%.o:
	$(CC) $(CFLAGS) $< -o $@

$(BIN_DIR)/dunyazad: $(CORE_OBJECTS) $(MAIN_OBJECTS) $(BIN_DIR)
	$(CC) $(CORE_OBJECTS) $(MAIN_OBJECTS) $(LFLAGS) -o $(BIN_DIR)/dunyazad

$(BIN_DIR)/unit_tests: $(CORE_OBJECTS) $(UNIT_TEST_OBJECTS) $(BIN_DIR) \
$(TEST_DIR)
	$(CC) $(CORE_OBJECTS) $(UNIT_TEST_OBJECTS) $(LFLAGS) -o $(BIN_DIR)/unit_tests
