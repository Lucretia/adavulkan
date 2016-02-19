TOP		=	../..
SRC		=	$(TOP)/src
DOC_SRC		=	$(TOP)/doc

GEN		=	gen
DOC_GEN		=	$(GEN)/doc

DESTDIR		?=	/opt

GPRBUILD	=	gprbuild
GPRINSTALL	=	gprinstall
GPRCLEAN	=	gprclean
INSTALL		=	install
CAT		=	cat

#########################################################################################################################

ifeq ($(VULKAN_BUILD),)
VULKAN_BUILD       =       static
endif

ifeq ($(VULKAN_MODE),)
VULKAN_MODE        =       debug
endif

ifeq ($(VULKAN_BUILD),static)
LIB_NAME        =       lib/libadavulkan.a
else
LIB_NAME        =       lib/libadavulkan.so.0.0.0
endif

#########################################################################################################################

.PHONY: vulkan.gpr

all: tools $(LIB_NAME) tests

# TODO: Fix the compiler so we actually get shared libs!
#

#####################################################################################
# Vulkan library

$(LIB_NAME): vulkan.gpr $(GEN_KB)
        $(GPRMAKE) -p -gnat2012 \
		-XVULKAN_BUILD=$(VULKAN_BUILD) -XVULKAN_MODE=$(VULKAN_MODE) -XVULKAN_PLATFORM=$(VULKAN_PLATFORM) \
                -Pvulkan.gpr

.PHONY: clean-lib

clean-lib:
        $(GPRCLEAN) -Pvulkan.gpr \
		-XVULKAN_BUILD=$(VULKAN_BUILD) -XVULKAN_MODE=$(VULKAN_MODE) -XVULKAN_PLATFORM=$(VULKAN_PLATFORM)

#####################################################################################
#####################################################################################
# Unit tests

unit_tests: $(LIB_NAME) build_unit_tests/unit_tests

build_unit_tests/unit_tests:
#        $(GPRMAKE) -p -gnat2012 \
#		-XVULKAN_BUILD=$(VULKAN_BUILD) -XVULKAN_MODE=$(VULKAN_MODE) -XVULKAN_PLATFORM=$(VULKAN_PLATFORM) \
#                -Punit_tests.gpr

.PHONY: clean-unit_tests

clean-unit_tests:
#        -$(GPRCLEAN) \
#		-XVULKAN_BUILD=$(VULKAN_BUILD) -XVULKAN_MODE=$(VULKAN_MODE) -XVULKAN_PLATFORM=$(VULKAN_PLATFORM) \
#		-Punit_tests.gpr

#####################################################################################
.PHONY: install

# TODO: Redo this to not use gprinstall, install our own gpr's.
install: all
        $(GPRINSTALL) --prefix=$(DESTDIR)/sdlada --build-name=$(VULKAN_MODE).$(VULKAN_BUILD) -p -f \
                -XVULKAN_BUILD=$(VULKAN_BUILD) -XVULKAN_MODE=$(VULKAN_MODE) -XVULKAN_PLATFORM=$(VULKAN_PLATFORM) \
                -Pvulkan.gpr

#####################################################################################

.PHONY: clean

clean: clean-unit_tests clean-lib
