/
/	File:	istartup.s
/	Author:	John van Groningen
/	Machine:	Intel 386

#define K6_0 0

#define d0 %eax
#define d1 %ebx
#define a0 %ecx
#define a1 %edx
#define a2 %ebp
#define a3 %esi
#define a4 %edi
#define sp %esp

#define d0w %ax
#define d1w %bx
#define a0w %cx
#define a1w %dx
#define a2w %bp
#define a3w %si
#define a4w %di

#define d0b %al
#define d1b %bl
#define a0b %cl
#define a1b %dl

#define d0lb %al
#define d0hb %ah
#define d1lb %bl
#define d1hb %bh

#define SHARE_CHAR_INT
#define MY_ITOS
#define FINALIZERS
#define STACK_OVERFLOW_EXCEPTION_HANDLER
#define WRITE_HEAP

#undef MEASURE_GC
#undef DEBUG
#undef PREFETCH2

#define NO_BIT_INSTRUCTIONS
#define ADJUST_HEAP_SIZE
#define MARK_GC
#define MARK_AND_COPY_GC

#define NEW_DESCRIPTORS

/ #define PROFILE
#define MODULE_NAMES_IN_TIME_PROFILER

#undef COMPACT_GC_ONLY

#define MINIMUM_HEAP_SIZE 8000
#define MINIMUM_HEAP_SIZE_2 4000

#if defined(_WINDOWS_) || defined (ELF)
# define	align(n) .align (1<<n)
#else
# define	align(n) .align n
#endif

#ifdef OS2
# define DLL
# define NOCLIB
#endif

#ifdef _WINDOWS_
# define NOCLIB
#endif

#ifdef LINUX
# define section(n) .section    .text.n,"ax"
#else
# define section(n) .text
#endif

#define DESCRIPTOR_ARITY_OFFSET	(-2)
#ifdef NEW_DESCRIPTORS
# define ZERO_ARITY_DESCRIPTOR_OFFSET	(-4)
#else
# define ZERO_ARITY_DESCRIPTOR_OFFSET	(-8)
#endif

	.comm	semi_space_size,4

	.globl	end_heap
	.comm	end_heap,4

	.comm	heap_mbp,4
	.comm	stack_mbp,4
	.comm	heap_p,4
	.comm	heap_p1,4
	.comm	heap_p2,4
	.comm	heap_p3,4
	.comm	neg_heap_p3,4
	.comm	end_heap_p3,4
	.comm	heap_size_33,4
	.comm	vector_p,4
	.comm	vector_counter,4
	.comm	neg_heap_vector_plus_4,4

	.comm	heap_size_32_33,4
	.comm	heap_vector,4
	.comm	stack_top,4
	.comm	end_vector,4

	.comm	heap_size_129,4
	.comm	heap_copied_vector,4
	.comm	heap_copied_vector_size,4
	.comm	heap_end_after_copy_gc,4

	.comm	heap_end_after_gc,4
	.comm	extra_heap,4
	.comm	extra_heap_size,4
	.comm	stack_p,4
	.comm	halt_sp,4
	
	.comm	n_allocated_words,4
	.comm	basic_only,4
#if !defined (OS2) && !defined (_WINDOWS_) && !defined (ELF)
	.comm	last_time,8
	.comm	execute_time,8
	.comm	garbage_collect_time,8
	.comm	IO_time,8
#else
	.comm	last_time,4
	.comm	execute_time,4
	.comm	garbage_collect_time,4
	.comm	IO_time,4
# ifdef MEASURE_GC
	.comm	compact_garbage_collect_time,4
	.comm	mark_compact_garbage_collect_time,4
	.comm	total_gc_bytes_lo,4
	.comm	total_gc_bytes_hi,4
	.comm	total_compact_gc_bytes_lo,4
	.comm	total_compact_gc_bytes_hi,4
# endif
#endif

	.globl	saved_heap_p
	.comm	saved_heap_p,4
	
	.globl	saved_a_stack_p
	.comm	saved_a_stack_p,4

	.globl	end_a_stack
	.comm	end_a_stack,4
	
	.globl	end_b_stack
	.comm	end_b_stack,4

	.comm	dll_initisialised,4

	.globl	int_to_real_scratch
	.comm	int_to_real_scratch,4	

#ifdef WRITE_HEAP
	.comm	heap_end_write_heap,4
	.comm	d3_flag_write_heap,4
	.comm	heap2_begin_and_end,8
#endif

#ifdef STACK_OVERFLOW_EXCEPTION_HANDLER
	.comm	a_stack_guard_page,4
#endif

	.globl	profile_stack_pointer
	.comm	profile_stack_pointer,4

	.data
	align	(2)

#ifdef MARK_GC
bit_counter:
	.long	0
bit_vector_p:
	.long	0
zero_bits_before_mark:
	.long	1
n_free_words_after_mark:
	.long	1000
n_last_heap_free_bytes:
	.long	0
lazy_array_list:
	.long	0
n_marked_words:
	.long	0
end_stack:
	.long	0
# ifdef ADJUST_HEAP_SIZE
bit_vector_size:
	.long	0
# endif
#endif

caf_list:
	.long	0
	.globl	caf_listp
caf_listp:
	.long	0
	
zero_length_string:
	.long	__STRING__+2
	.long	0
true_string:
	.long	__STRING__+2
	.long	4
true_c_string:
	.ascii	"True"
	.byte	0,0,0,0
false_string:
	.long	__STRING__+2
	.long	5
false_c_string:
	.ascii	"False"
	.byte	0,0,0
file_c_string:
	.ascii	"File"
	.byte	0,0,0,0
garbage_collect_flag:
	.byte	0
	.byte	0,0,0

	.comm	sprintf_buffer,32

out_of_memory_string_1:
	.ascii	"Not enough memory to allocate heap and stack"
	.byte	10,0
printf_int_string:
	.ascii	"%d"
	.byte	0
printf_real_string:
	.ascii	"%.15g"
	.byte	0
printf_string_string:
	.ascii	"%s"
	.byte	0
printf_char_string:
	.ascii	"%c"
	.byte	0
garbage_collect_string_1:
	.asciz	"A stack: "
garbage_collect_string_2:
	.asciz	" bytes. BC stack: "
garbage_collect_string_3:
	.ascii	" bytes."
	.byte	10,0
heap_use_after_gc_string_1:
	.ascii	"Heap use after garbage collection: "
	.byte	0
heap_use_after_gc_string_2:
	.ascii	" Bytes."
	.byte	10,0
stack_overflow_string:
	.ascii	"Stack overflow."
	.byte	10,0
out_of_memory_string_4:
	.ascii	"Heap full."
	.byte	10,0
time_string_1:
	.ascii	"Execution: "
	.byte	0
time_string_2:
	.ascii	"  Garbage collection: "
	.byte	0
#ifdef MEASURE_GC
time_string_3:
	.ascii	" "
	.byte	0
#endif
time_string_4:
	.ascii	"  Total: "
	.byte	0
high_index_string:
	.ascii	"Index too high in UPDATE string."
	.byte	10,0
low_index_string:
	.ascii	"Index negative in UPDATE string."
	.byte	10,0
IO_error_string:
	.ascii	"IO error: "
	.byte	0
new_line_string:
	.byte	10,0
	
sprintf_time_string:
	.ascii	"%d.%02d"
	.byte	0

#ifdef MARK_GC
marked_gc_string_1:
	.ascii	"Marked: "
	.byte	0
#endif
#ifdef PROFILE
	align	(2)
# ifdef MODULE_NAMES_IN_TIME_PROFILER
#  ifdef LINUX
	.globl	m_system
#  endif
m_system:
	.long	6
	.ascii	"System"
	.byte	0
	.byte	0
	.long	m_system

# endif
garbage_collector_name:
	.long	0
	.asciz	"garbage_collector"
	align	(2)
#endif


#ifdef NOCLIB
	align	(3)
NAN_real:
	.long	0xffffffff,0x7fffffff
one_real:
	.long	0x00000000,0x3ff00000
zero_real:
	.long   0x00000000,0x00000000
#endif

#ifdef NO_BIT_INSTRUCTIONS
	align	(2)
bit_set_table:
	.long	0x00000001,0x00000002,0x00000004,0x00000008
	.long	0x00000010,0x00000020,0x00000040,0x00000080
	.long	0x00000100,0x00000200,0x00000400,0x00000800
	.long	0x00001000,0x00002000,0x00004000,0x00008000
	.long	0x00010000,0x00020000,0x00040000,0x00080000
	.long	0x00100000,0x00200000,0x00400000,0x00800000
	.long	0x01000000,0x02000000,0x04000000,0x08000000
	.long	0x10000000,0x20000000,0x40000000,0x80000000
	.long	0
bit_clear_table:
	.long	0xfffffffe,0xfffffffd,0xfffffffb,0xfffffff7
	.long	0xffffffef,0xffffffdf,0xffffffbf,0xffffff7f
	.long	0xfffffeff,0xfffffdff,0xfffffbff,0xfffff7ff
	.long	0xffffefff,0xffffdfff,0xffffbfff,0xffff7fff
	.long	0xfffeffff,0xfffdffff,0xfffbffff,0xfff7ffff
	.long	0xffefffff,0xffdfffff,0xffbfffff,0xff7fffff
	.long	0xfeffffff,0xfdffffff,0xfbffffff,0xf7ffffff
	.long	0xefffffff,0xdfffffff,0xbfffffff,0x7fffffff
	.long	0xffffffff
first_one_bit_table:
	.byte	-1,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	7,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	6,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
	.byte	4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0
#endif

#ifdef DLL
start_address:
	.long	0
#endif
	align	(2)
	.comm	sprintf_time_buffer,20

	align	(2)
#ifdef SHARE_CHAR_INT
	.globl	small_integers
	.comm	small_integers,33*8
	.globl	static_characters
	.comm	static_characters,256*8
#endif

	.text

	.globl	@abc_main
	.globl	print
	.globl	print_char
	.globl	print_int
	.globl	print_real
	.globl	print__string__
	.globl	print__chars__sc
	.globl	print_sc
	.globl	print_symbol
	.globl	print_symbol_sc
	.globl	printD
	.globl	DtoAC
	.globl	push_t_r_args
	.globl	push_a_r_args
	.globl	halt
	.globl	dump

	.globl	catAC
	.globl	sliceAC
	.globl	updateAC
	.globl	eqAC
	.globl	cmpAC

	.globl	string_to_string_node
	.globl	int_array_to_node
	.globl	real_array_to_node

	.globl	_create_arrayB
	.globl	_create_arrayC
	.globl	_create_arrayI
	.globl	_create_arrayR
	.globl	_create_r_array
	.globl	create_array
	.globl	create_arrayB
	.globl	create_arrayC
	.globl	create_arrayI
	.globl	create_arrayR
	.globl	create_R_array

	.globl	BtoAC
	.globl	ItoAC
	.globl	RtoAC
	.globl	eqD

	.globl	collect_0
	.globl	collect_1
	.globl	collect_2

	.globl	collect_0l
	.globl	collect_1l
	.globl	collect_2l

	.globl	yet_args_needed
	.globl	yet_args_needed_0
	.globl	yet_args_needed_1
	.globl	yet_args_needed_2
	.globl	yet_args_needed_3
	.globl	yet_args_needed_4

	.globl	_c3,_c4,_c5,_c6,_c7,_c8,_c9,_c10,_c11,_c12
	.globl	_c13,_c14,_c15,_c16,_c17,_c18,_c19,_c20,_c21,_c22
	.globl	_c23,_c24,_c25,_c26,_c27,_c28,_c29,_c30,_c31,_c32

	.globl	e__system__nind
	.globl	e__system__eaind
/ old names of the previous two labels for compatibility, remove later
	.globl	__indirection,__eaind
	.globl	e__system__dind
	.globl	eval_fill

	.globl	eval_upd_0,eval_upd_1,eval_upd_2,eval_upd_3,eval_upd_4
	.globl	eval_upd_5,eval_upd_6,eval_upd_7,eval_upd_8,eval_upd_9
	.globl	eval_upd_10,eval_upd_11,eval_upd_12,eval_upd_13,eval_upd_14
	.globl	eval_upd_15,eval_upd_16,eval_upd_17,eval_upd_18,eval_upd_19
	.globl	eval_upd_20,eval_upd_21,eval_upd_22,eval_upd_23,eval_upd_24
	.globl	eval_upd_25,eval_upd_26,eval_upd_27,eval_upd_28,eval_upd_29
	.globl	eval_upd_30,eval_upd_31,eval_upd_32

	.globl	repl_args_b
	.globl	push_arg_b
	.globl	del_args
#if 0
	.globl	o__S_P2
	.globl	ea__S_P2
#endif
	.globl	add_IO_time
	.globl	add_execute_time
	.globl	@IO_error
	.globl	stack_overflow

	.globl	out_of_memory_4
	.globl	print_error
#ifndef DLL
	.global	_start
#endif
	.globl	tan_real
	.globl	asin_real
	.globl	acos_real
	.globl	atan_real
	.globl	ln_real
	.globl	log10_real
	.globl	exp_real
	.globl	pow_real
	.globl	r_to_i_real
	.globl	truncate_real
	.globl	entier_real
	.globl	ceiling_real
	.globl	round__real64
	.globl	truncate__real64
	.globl	entier__real64
	.globl	ceiling__real64
	.globl	int64a__to__real

#ifdef NOCLIB
	.globl	@c_pow
	.globl	@c_log10
	.globl	@c_entier
#endif
#ifdef PROFILE
	.globl	init_profiler
	.globl	profile_s,profile_n,profile_r,profile_t
	.globl	write_profile_information,write_profile_stack
#endif
	.globl	__driver

/ from system.abc:	
	.global	INT
	.global	CHAR
	.global	BOOL
	.global	REAL
	.global	FILE
	.global	__STRING__
	.global	__ARRAY__
	.global	__cycle__in__spine
	.global	__print__graph
	.global	__eval__to__nf

/ from wcon.c:
	.globl	@w_print_char
	.globl	@w_print_string
	.globl	@w_print_text
	.globl	@w_print_int
	.globl	@w_print_real
	
	.globl	@ew_print_char
	.globl	@ew_print_text
	.globl	@ew_print_string
	.globl	@ew_print_int

	.global	@ab_stack_size
	.global	@heap_size
	.global	@flags

/ from standard c library:
#ifdef USE_CLIB
	.globl	@malloc
	.globl	@free
	.globl	@sprintf
	.globl	@strlen
#else
	.globl	@allocate_memory
# ifdef STACK_OVERFLOW_EXCEPTION_HANDLER
	.globl	@allocate_memory_with_guard_page_at_end
# endif
	.globl	@free_memory
#endif

#ifdef ADJUST_HEAP_SIZE
	.global	@heap_size_multiple
	.global	@initial_heap_size
#endif
#ifdef WRITE_HEAP
	.global	@min_write_heap_size
#endif
#ifdef FINALIZERS
	.global	__Nil
	.globl	finalizer_list
	.comm	finalizer_list,4
	.globl	free_finalizer_list
	.comm	free_finalizer_list,4
#endif

@abc_main:
	push	d1
	push	a0
	push	a1
	push	a2
	push	a3
	push	a4

	finit
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz

#ifdef DLL
	mov	28(sp),d0
	mov	d0,start_address
#endif
	call	init_clean
	test	%eax,%eax
	jne	init_error

	call	init_timer

	mov	sp,halt_sp

#ifdef PROFILE
	call	init_profiler
#endif

#ifdef DLL
	mov	start_address,d0
	call	*d0
#else
# ifdef ELF
	call	__start
# else
	call	_start
# endif
#endif

exit:
	call	exit_clean

init_error:
	pop	a4
	pop	a3
	pop	a2
	pop	a1
	pop	a0
	pop	d1
	ret

#if defined (_WINDOWS_) || defined (LINUX)
# ifdef _WINDOWS_
	.globl	@DllMain?12
@DllMain?12:
	cmpl	$1,8(sp)
	je	DLL_PROCESS_ATTACH
	jb	DLL_PROCESS_DETACH
	ret	$12

DLL_PROCESS_ATTACH:
# else
	.globl	clean_init
clean_init:
# endif
	push	d1
	push	a0
	push	a1
	push	a2
	push	a3
	push	a4

	movl	$1,dll_initisialised

	call	init_clean
	test	%eax,%eax
	jne	init_dll_error

	call	init_timer

	mov	sp,halt_sp

# ifdef PROFILE
	call	init_profiler
# endif

	mov	%edi,saved_heap_p
	mov	%esi,saved_a_stack_p

	movl	$1,%eax
	jmp	exit_dll_init

init_dll_error:
	xor	%eax,%eax
	jmp	exit_dll_init
# ifdef _WINDOWS_
DLL_PROCESS_DETACH:
# else
	.globl	clean_fini
clean_fini:
# endif
	push	d1
	push	a0
	push	a1
	push	a2
	push	a3
	push	a4
	
	mov	saved_heap_p,%edi
	mov	saved_a_stack_p,%esi

	call	exit_clean

exit_dll_init:
	pop	a4
	pop	a3
	pop	a2
	pop	a1
	pop	a0
	pop	d1
# ifdef _WINDOWS_
	ret	$12
# else
	ret
# endif
#endif

init_clean:
	lea	128(sp),d0
	subl	@ab_stack_size,d0
	movl	d0,end_b_stack

	mov	@flags,d0
	andl	$1,d0
	mov	d0,basic_only

	movl	@heap_size,d0
#ifdef PREFETCH2
	sub	$63,d0
#else
	sub	$3,d0
#endif
	xorl	a1,a1
	mov	$33,d1
	div	d1
	movl	d0,heap_size_33

	movl	@heap_size,d0
	sub	$3,d0
	xorl	a1,a1
	mov	$129,d1
	div	d1
	mov	d0,heap_size_129
	add	$3,d0
	andl	$-4,d0
	movl	d0,heap_copied_vector_size
	movl	$0,heap_end_after_copy_gc

	movl	@heap_size,d0
	add	$7,d0
	andl	$-8,d0
	movl	d0,@heap_size
	add	$7,d0

	push	d0
#ifdef USE_CLIB
	call	@malloc
#else
	call	@allocate_memory
#endif
	add	$4,sp
	
	test	d0,d0
	je	no_memory_2

	mov	d0,heap_mbp
	lea	3(d0),a4
	andl	$-4,a4
	mov	a4,heap_p

	mov	@ab_stack_size,a2
	add	$3,a2

	push	a2
#ifdef STACK_OVERFLOW_EXCEPTION_HANDLER
	call	@allocate_memory_with_guard_page_at_end
#else
# ifdef USE_CLIB
	call	@malloc
# else
	call	@allocate_memory
# endif
#endif
	add	$4,sp
	
	test	d0,d0
	je	no_memory_3
	
	mov	d0,stack_mbp
#ifdef STACK_OVERFLOW_EXCEPTION_HANDLER
	addl	@ab_stack_size,d0
	addl	$3+4095,d0
	andl	$-4096,d0
	movl	d0,a_stack_guard_page
	subl	@ab_stack_size,d0
#endif
	add	$3,d0
	andl	$-4,d0

	mov	d0,a3
	mov	d0,stack_p

	addl	@ab_stack_size,d0
	subl	$64,d0
	movl	d0,end_a_stack

#ifdef SHARE_CHAR_INT
	leal	small_integers,a0
	xorl	d0,d0
	leal	INT+2,d1

make_small_integers_lp:
	mov	d1,(a0)
	mov	d0,4(a0)
	inc	d0
	add	$8,a0
	cmp	$33,d0
	jne	make_small_integers_lp

	leal	static_characters,a0
	xorl	d0,d0
	leal	CHAR+2,d1

make_static_characters_lp:
	mov	d1,(a0)
	mov	d0,4(a0)
	inc	d0
	add	$8,a0
	cmp	$256,d0
	jne	make_static_characters_lp
#endif

	lea	caf_list+4,a0
	movl	a0,caf_listp

#ifdef FINALIZERS
	movl	$__Nil-4,finalizer_list
	movl	$__Nil-4,free_finalizer_list
#endif

	mov	a4,heap_p1

	movl	heap_size_129,a2
	shl	$4,a2
	lea	(a4,a2,4),d0
	mov	d0,heap_copied_vector
	add	heap_copied_vector_size,d0
	mov	d0,heap_p2

	movb	$0,garbage_collect_flag

# ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	je	no_mark1
# endif

# if defined (MARK_GC) || defined (COMPACT_GC_ONLY)
	movl	heap_size_33,d0
	movl	a4,heap_vector
	addl	d0,a4
#  ifdef PREFETCH2
	addl	$63,a4
	andl	$-64,a4
#  else
	addl	$3,a4
	andl	$-4,a4
#  endif
	movl	a4,heap_p3
	lea	(,d0,8),a2
	movb	$-1,garbage_collect_flag
# endif

# ifdef MARK_AND_COPY_GC
no_mark1:
# endif

# ifdef ADJUST_HEAP_SIZE
	movl	@initial_heap_size,d0
#  ifdef MARK_AND_COPY_GC
	movl	$(MINIMUM_HEAP_SIZE_2),d1
	testb	$64,@flags
	jne	no_mark9
	addl	d1,d1
no_mark9:
#  else
#   if defined (MARK_GC) || defined (COMPACT_GC_ONLY)
	movl	$(MINIMUM_HEAP_SIZE),d1
#   else
	movl	$(MINIMUM_HEAP_SIZE_2),d1
#   endif
#  endif

	cmpl	d1,d0
	jle	too_large_or_too_small
	shr	$2,d0
	cmpl	a2,d0
	jge	too_large_or_too_small
	movl	d0,a2
too_large_or_too_small:
# endif

	lea	(a4,a2,4),d0
	mov	d0,heap_end_after_gc
	subl	$32,d0
	movl	d0,end_heap

# ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	je	no_mark2
# endif

# if defined (MARK_GC) && defined (ADJUST_HEAP_SIZE)
	movl	a2,bit_vector_size
# endif

# ifdef MARK_AND_COPY_GC
no_mark2:
# endif

	xor	%eax,%eax
	ret

no_memory_2:
	push	$out_of_memory_string_1
	call	@ew_print_string
	add	$4,sp
	movl	$1,@execution_aborted
	movl	$1,%eax
	ret

no_memory_3:
	push	$out_of_memory_string_1
	call	@ew_print_string
	add	$4,sp
	movl	$1,@execution_aborted

	push	heap_mbp
#ifdef USE_CLIB
	call	@free
#else
	call	@free_memory
#endif
	add	$4,sp

	movl	$1,%eax
	ret

exit_clean:
	call	add_execute_time

	mov	@flags,d0
	testb	$8,d0b
	je	no_print_execution_time
	
	push	$time_string_1
	call	@ew_print_string
	add	$4,sp
	
	mov	execute_time,d0
#if !defined (OS2) && !defined (_WINDOWS_) && !defined (ELF)
	mov	execute_time+4,d1
#endif
	call	print_time
	
	push	$time_string_2
	call	@ew_print_string
	add	$4,sp

	mov	garbage_collect_time,d0
#if !defined (OS2) && !defined (_WINDOWS_) && !defined (ELF)
	mov	garbage_collect_time+4,d1
#endif
	call	print_time

#ifdef MEASURE_GC
	push	$time_string_3
	call	@ew_print_string
	add	$4,sp

	mov	mark_compact_garbage_collect_time,d0
# if !defined (OS2) && !defined (_WINDOWS_)
	mov	mark_compact_garbage_collect_time+4,d1
# endif
	call	print_time

	push	$time_string_3
	call	@ew_print_string
	add	$4,sp

	mov	compact_garbage_collect_time,d0
# if !defined (OS2) && !defined (_WINDOWS_)
	mov	compact_garbage_collect_time+4,d1
# endif
	call	print_time
#endif

	push	$time_string_4
	call	@ew_print_string
	add	$4,sp

#if !defined (OS2) && !defined (_WINDOWS_) && !defined (ELF)
	mov	execute_time,d0
	mov	execute_time+4,d1

	add	garbage_collect_time,d0
	add	garbage_collect_time+4,d1
	cmp	$1000000,d1
	jb	no_usec_overflow_1
	sub	$1000000,d1
	inc	d0
no_usec_overflow_1:

	add	IO_time,d0
	add	IO_time+4,d1
	cmp	$1000000,d1
	jb	no_usec_overflow_2
	sub	$1000000,d1
	inc	d0
no_usec_overflow_2:
#else
	mov	execute_time,d0
	add	garbage_collect_time,d0
	add	IO_time,d0
# ifdef MEASURE_GC
	add	mark_compact_garbage_collect_time,d0
	add	compact_garbage_collect_time,d0
# endif
#endif

	call	print_time

#ifdef MEASURE_GC
	push	$10
	call	@ew_print_char
	addl	$4,sp

	pushl	total_gc_bytes_hi
	call	@ew_print_int
	addl	$4,sp

	push	$':'
	call	@ew_print_char
	addl	$4,sp

	pushl	total_gc_bytes_lo
	call	@ew_print_int
	addl	$4,sp

	push	$32
	call	@ew_print_char
	addl	$4,sp

	pushl	total_compact_gc_bytes_hi
	call	@ew_print_int
	addl	$4,sp

	push	$':'
	call	@ew_print_char
	addl	$4,sp

	pushl	total_compact_gc_bytes_lo
	call	@ew_print_int
	addl	$4,sp
#endif

	push	$10
	call	@ew_print_char
	add	$4,sp

no_print_execution_time:
	push	stack_mbp
#ifdef USE_CLIB
	call	@free
#else
	call	@free_memory
#endif
	add	$4,sp

	push	heap_mbp
#ifdef USE_CLIB
	call	@free
#else
	call	@free_memory
#endif
	add	$4,sp

#ifdef PROFILE
	call	write_profile_information
#endif

	ret

__driver:
	mov	@flags,a2
	test	$16,a2
	je	__print__graph
	jmp	__eval__to__nf


print_time:
#if !defined (OS2) && !defined (_WINDOWS_) && !defined (ELF)
	mov	d0,a0
	xorl	a1,a1
	mov	d1,d0
	mov	$10000,d1
	div	d1
#else
	xorl	a1,a1
	movl	$1000,d1
	div	d1
	movl	d0,a0
	movl	a1,d0
	xorl	a1,a1
	movl	$10,d1
	div	d1
#endif
	push	d0
	push	a0

#ifdef USE_CLIB
	push	$sprintf_time_string
	push	$sprintf_time_buffer
	call	@sprintf
	add	$16,sp

	push	$sprintf_time_buffer
	call	@ew_print_string
	add	$4,sp
#else
	call	@ew_print_int

	add	$4,sp
	movl	$sprintf_time_buffer,a0

	xorl	a1,a1
	mov	$10,d1

/	movb	$'.',(a0)
	movb	$46,(a0)
	pop	d0

	div	d1
	add	$48,d0
	add	$48,a1
	movb	d0b,1(a0)
	movb	a1b,2(a0)

	push	$3
	push	a0
	call	@ew_print_text
	add	$8,sp
#endif
	ret

print_sc:
	mov	basic_only,a2
	test	a2,a2
	jne	end_print

print:
	push	d0
	call	@w_print_string
	add	$4,sp

end_print:
	ret

dump:
	call	print
	jmp	halt

printD:	testb	$2,d0b
	jne	printD_
	
	mov	d0,a2
	jmp	print_string_a2

DtoAC_record:
#ifdef NEW_DESCRIPTORS
	movl	-6(d0),a2
#else
	movl	-4(a2),a2
#endif
	jmp	DtoAC_string_a2

DtoAC:	testb	$2,d0b
	jne	DtoAC_

	mov	d0,a2
	jmp	DtoAC_string_a2

DtoAC_:
#ifdef NEW_DESCRIPTORS
	cmpw	$256,-2(d0)
	jae	DtoAC_record

  	movzwl	(d0),d1
  	lea	10(d0,d1),a2
#else
	movswl	-2(d0),d1
	lea	-2(d0),a2
	cmp	$256,d1
	jae	DtoAC_record

	shl	$3,d1
	sub	d1,a2

 	movzwl	DESCRIPTOR_ARITY_OFFSET(a2),d1
	lea	4(a2,d1,8),a2
#endif

DtoAC_string_a2:
	movl	(a2),d0
	lea	4(a2),a0
	jmp	build_string

print_symbol:
	xorl	d1,d1
	jmp	print_symbol_2

print_symbol_sc:
	mov	basic_only,d1
print_symbol_2:
	mov	(a0),d0
	
	cmp	$INT+2,d0
	je	print_int_node

	cmp	$CHAR+2,d0
	je	print_char_denotation

	cmp	$BOOL+2,d0
	je	print_bool

	cmp	$REAL+2,d0
	je	print_real_node
	
	test	d1,d1
	jne	end_print_symbol

printD_:
#ifdef NEW_DESCRIPTORS
	cmpw	$256,-2(d0)
	jae	print_record

  	movzwl	(d0),d1
  	lea	10(d0,d1),a2
	jmp	print_string_a2

print_record:
	movl	-6(d0),a2
	jmp	print_string_a2
#else
	movswl	-2(d0),d1
	lea	-2(d0),a2
	cmp	$256,d1
	jae	no_print_record

	shl	$3,d1
	sub	d1,a2

  	movzwl	DESCRIPTOR_ARITY_OFFSET(a2),d1
	lea	4(a2,d1,8),a2
	jmp	print_string_a2

no_print_record:
	mov	-4(a2),a2
	jmp	print_string_a2
#endif

end_print_symbol:
	ret

print_int_node:
	push	4(a0)
	call	@w_print_int
	add	$4,sp
	ret

print_int:
	push	d0
	call	@w_print_int
	add	$4,sp
	ret

print_char_denotation:
	test	d1,d1
	jne	print_char_node

	push	4(a0)

	push	$0x27
	call	@w_print_char
	add	$4,sp
	
	call	@w_print_char
	add	$4,sp

	push	$0x27
	call	@w_print_char
	add	$4,sp

	ret

print_char_node:
	push	4(a0)
	call	@w_print_char
	add	$4,sp
	ret
	
print_char:
	push	d0
	call	@w_print_char
	add	$4,sp
	ret

print_bool:
	movsbl	4(a0),a0
	test	a0,a0
	je	print_false

print_true:
	push	$true_c_string
	call	@w_print_string
	add	$4,sp
	ret

print_false:
	push	$false_c_string
	call	@w_print_string
	add	$4,sp
	ret

print_real:
	subl	$8,sp
	fstpl	0(sp)
	jmp	print_real_
print_real_node:
	push	8(a0)
	push	4(a0)
print_real_:
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@w_print_real
	add	$8,sp
	ret

print_string_a2:
	add	$4,a2
	push	-4(a2)
	push	a2
	call	@w_print_text
	add	$8,sp
	ret

print__chars__sc:
	mov	basic_only,a2
	test	a2,a2
	jne	no_print_chars

print__string__:
	push	4(a0)
	lea	8(a0),a2
	push	a2
	call	@w_print_text
	add	$8,sp
no_print_chars:
	ret

push_a_r_args:
	pushl	a4

	movl	8(a0),a1
	subl	$2,a1
	movzwl	(a1),a4
	subl	$256,a4
	movzwl	2(a1),d1
	addl	$4,a1
	pushl	a1
		
	movl	a4,a1
	subl	d1,a1

	shl	$2,d0
	lea	12(a0,d1,4),a0
	dec	a4
mul_array_size_lp:
	addl	d0,a0
	subl	$1,a4
	jnc	mul_array_size_lp

	lea	(a0,a1,4),a4
	jmp	push_a_elements
push_a_elements_lp:
	movl	-4(a0),d0
	subl	$4,a0
	movl	d0,K6_0(a3)
	addl	$4,a3
push_a_elements:
	subl	$1,d1
	jnc	push_a_elements_lp

	movl	a4,a0
	popl	d0
	popl	a4

	popl	a2
	jmp	push_b_elements
push_b_elements_lp:
	pushl	-4(a0)
	subl	$4,a0
push_b_elements:
	subl	$1,a1
	jnc	push_b_elements_lp

	jmp	*a2

push_t_r_args:
	popl	a2

	movl	(a0),a1
	addl	$4,a0
	subl	$2,a1
	movzwl	(a1),d0
	subl	$256,d0
	movzwl	2(a1),d1
	addl	$4,a1

	movl	a1,K6_0(a3)
	movl	d1,4(a3)
	
	subl	d0,d1
	negl	d1

	lea	(a0,d0,4),a1
	cmpl	$2,d0
	jbe	small_record
	movl	4(a0),a1
	lea	-4(a1,d0,4),a1
small_record:
	jmp	push_r_b_elements

push_r_b_elements_lp:
	dec	d0
	jne	not_first_arg_b
	
	pushl	(a0)
	jmp	push_r_b_elements
not_first_arg_b:
	pushl	-4(a1)
	subl	$4,a1
push_r_b_elements:
	subl	$1,d1
	jnc	push_r_b_elements_lp

	movl	4(a3),d1
	pushl	a2
	pushl	K6_0(a3)
	jmp	push_r_a_elements

push_r_a_elements_lp:
	dec	d0
	jne	not_first_arg_a
	
	movl	(a0),a2
	movl	a2,K6_0(a3)
	addl	$4,a3
	jmp	push_r_a_elements
not_first_arg_a:
	movl	-4(a1),a2
	subl	$4,a1
	movl	a2,K6_0(a3)
	addl	$4,a3
push_r_a_elements:
	subl	$1,d1
	jnc	push_r_a_elements_lp

	popl	d0
	ret

BtoAC:
	testb	d0b,d0b
	je	BtoAC_false
BtoAC_true:
	mov	$true_string,a0
	ret
BtoAC_false:
	mov	$false_string,a0
	ret

RtoAC:
#ifndef USE_CLIB
	push	$sprintf_buffer	
#endif
	subl	$8,sp
	fstl	0(sp)
	
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	
#ifdef USE_CLIB
	push	$printf_real_string
	push	$sprintf_buffer	
	call	@sprintf
	add	$16,sp
#else
	call	@convert_real_to_string
	add	$12,sp
#endif
	jmp	return_sprintf_buffer

ItoAC:
#ifdef MY_ITOS
	mov	$sprintf_buffer,a0
	call	int_to_string
	
	movl	a0,d0
	subl	$sprintf_buffer,d0

	jmp	sprintf_buffer_to_string

# ifdef NOCLIB
	.globl	@convert_int_to_string
@convert_int_to_string:
	push	a0
	push	a1
	push	a2
	push	d1
	movl	16+4(sp),a0
	movl	16+8(sp),d0
	call	int_to_string
	movl	a0,d0
	pop	d1
	pop	a2
	pop	a1
	pop	a0
	ret
# endif

int_to_string:
	test	d0,d0
	jns	no_minus
	movb	$45,(a0)
	inc	a0
	neg	d0
no_minus:
	lea	12(a0),a2

	je	zero_digit

#ifdef USE_DIV
	movl	$10,d1
#endif

calculate_digits:
#ifndef USE_DIV
	cmp	$10,d0
#else
	cmp	d1,d0
#endif
	jb	last_digit

#ifndef USE_DIV
	movl	$0xcccccccd,a1 
	movl	d0,d1

	mull	a1 

	movl	a1,d0 
	andl	$-8,a1 
	add	$48,d1

	shrl	$3,d0 
	subl	a1,d1
	shrl	$2,a1 

	subl	a1,d1
	movb	d1b,(a2)
#else
	xorl	a1,a1
	div	d1
	add	$48,a1
	movb	a1b,(a2)
#endif
	inc	a2
	jmp	calculate_digits

last_digit:
	test	d0,d0
	je	no_zero
zero_digit:
	add	$48,d0
	movb	d0b,(a2)
	inc	a2
no_zero:
	lea	12(a0),a1

reverse_digits:
	movb	-1(a2),d1b
	dec	a2
	movb	d1b,(a0)
	inc	a0
	cmp	a2,a1
	jne	reverse_digits

	movb	$0,(a0)
	ret
#else
	push	d0
	push	$printf_int_string
	push	$sprintf_buffer
	call	@sprintf
	add	$12,sp
#endif

return_sprintf_buffer:
#ifdef USE_CLIB
	push	$sprintf_buffer
	call	@strlen
	add	$4,sp
#else
	mov	$sprintf_buffer-1,d0
skip_characters:
	inc	d0
	cmpb	$0,(d0)
	jne	skip_characters

	sub	$sprintf_buffer,d0
#endif

#ifdef MY_ITOS
sprintf_buffer_to_string:
	mov	$sprintf_buffer,a0
build_string:
#endif
	lea	3(d0),d1
	shr	$2,d1
	add	$2,d1

	lea	-32(a4,d1,4),a2
	cmpl	end_heap,a2
	jb	D_to_S_no_gc

	push	a0
	call	collect_0l
	pop	a0

D_to_S_no_gc:
	sub	$2,d1
	mov	a4,a2
	movl	$__STRING__+2,(a4)
	mov	d0,4(a4)
	add	$8,a4
	jmp	D_to_S_cp_str_2

D_to_S_cp_str_1:
	mov	(a0),d0
	add	$4,a0
	mov	d0,(a4)
	add	$4,a4
D_to_S_cp_str_2:
	sub	$1,d1
	jnc	D_to_S_cp_str_1
	
	movl	a2,a0
	ret

eqD:	mov	(a0),d0
	cmp	(a1),d0
	jne	eqD_false

	cmp	$INT+2,d0
	je	eqD_INT
	cmp	$CHAR+2,d0
	je	eqD_CHAR
	cmp	$BOOL+2,d0
	je	eqD_BOOL
	cmp	$REAL+2,d0
	je	eqD_REAL

	mov	$1,d0
	ret

eqD_CHAR:
eqD_INT:	mov	4(a0),d1
	xorl	d0,d0
	cmp	4(a1),d1
	sete	%al
	ret

eqD_BOOL:	movb	4(a0),d1b
	xorl	d0,d0
	cmpb	4(a1),d1b
	sete	d0b
	ret

eqD_REAL:	
	fldl	4(a0)
	fcompl	4(a1)
	fnstsw	%ax
	andb	$68,%ah
	xorb	$64,%ah
	sete	%al
	andl	$1,%eax
	ret

eqD_false:
	xorl	d0,d0
	ret
/
/	the timer
/

#if !defined (OS2) && !defined (_WINDOWS_) && !defined (ELF)
init_timer:
	sub	$88,sp
	push	sp
	push	$0
	call	@getrusage
	add	$8,sp
	
	mov	(sp),d0
	mov	4(sp),d1
	mov	d0,last_time
	mov	d1,last_time+4
	xorl	d0,d0
	mov	d0,execute_time
	mov	d0,execute_time+4
	mov	d0,garbage_collect_time
	mov	d0,garbage_collect_time+4
	mov	d0,IO_time
	mov	d0,IO_time+4
	add	$88,sp
	ret

get_time_diff:
	sub	$88,sp
	push	sp
	push	$0
	call	@getrusage
	add	$8,sp

	mov	(sp),d0
	mov	4(sp),d1

	mov	$last_time,a0
	mov	(a0),a1
	mov	d0,(a0)
	sub	a1,d0

	mov	4(a0),a1
	mov	d1,4(a0)

	sub	a1,d1
	jae	get_time_diff_1
	add	$1000000,d1
	dec	d0
get_time_diff_1:
	add	$88,sp
	ret
	
add_execute_time:
	push	d1

	call	get_time_diff
	
	mov	$execute_time,a0

add_time:
	add	(a0),d0
	add	4(a0),d1
	cmp	$1000000,d1
	jb	add_execute_time_1
	sub	$1000000,d1
	inc	d0
add_execute_time_1:
	mov	d0,(a0)
	mov	d1,4(a0)
	pop	d1
	ret

add_garbage_collect_time:
	push	d1
	call	get_time_diff

	mov	$garbage_collect_time,a0
	jmp	add_time

add_IO_time:
	push	d1
	call	get_time_diff

	mov	$IO_time,a0
	jmp	add_time
#else

init_timer:
#ifdef _WINDOWS_
	call	_GetTickCount?0
#else
# ifdef ELF
	subl    $20,sp
	push    sp
	call    times
	addl    $4,sp
	movl    (sp),d0
	imul    $10,d0
	addl    $20,sp
# else
	subl	$4,sp
	pushl	$4
	lea	4(sp),a0
	pushl	a0
	pushl	$14
	pushl	$14
	call	_DosQuerySysInfo
	addl	$16,sp
	popl	d0
# endif
#endif
	mov	d0,last_time
	xorl	d0,d0
	mov	d0,execute_time
	mov	d0,garbage_collect_time
	mov	d0,IO_time
#ifdef MEASURE_GC
	mov	d0,mark_compact_garbage_collect_time
	mov	d0,compact_garbage_collect_time
#endif
	ret

get_time_diff:
#ifdef _WINDOWS_
	call	_GetTickCount?0
#else
# ifdef ELF
	subl    $20,sp
	push    sp
	call    times
	addl    $4,sp
	movl    (sp),d0
	imul    $10,d0
	addl    $20,sp
# else
	subl	$4,sp
	pushl	$4
	lea	4(sp),a0
	pushl	a0
	pushl	$14
	pushl	$14
	call	_DosQuerySysInfo
	addl	$16,sp
	popl	d0
# endif
#endif
	mov	$last_time,a0
	mov	(a0),a1
	mov	d0,(a0)
	sub	a1,d0
	ret
	
add_execute_time:
	call	get_time_diff
	mov	$execute_time,a0

add_time:
	add	(a0),d0
	mov	d0,(a0)
	ret

add_garbage_collect_time:
	call	get_time_diff
	mov	$garbage_collect_time,a0
	jmp	add_time

add_IO_time:
	call	get_time_diff
	mov	$IO_time,a0
	jmp	add_time

# ifdef MEASURE_GC
add_mark_compact_garbage_collect_time:
	call	get_time_diff
	mov	$mark_compact_garbage_collect_time,a0
	jmp	add_time

add_compact_garbage_collect_time:
	call	get_time_diff
	mov	$compact_garbage_collect_time,a0
	jmp	add_time
# endif
#endif

/
/	the garbage collector
/

collect_2l:
#ifdef PROFILE
	pushl	a2
	movl	$garbage_collector_name,a2
	call	profile_s
	popl	a2
#endif
	mov	a0,K6_0(a3)
	mov	a1,4(a3)
	add	$8,a3
	call	collect_0l_
	mov	-4(a3),a1
	mov	-8(a3),a0
	sub	$8,a3
#ifdef PROFILE
	jmp	profile_r
#else
	ret
#endif

collect_1l:
#ifdef PROFILE
	pushl	a2
	movl	$garbage_collector_name,a2
	call	profile_s
	popl	a2
#endif
	mov	a0,K6_0(a3)
	add	$4,a3
	call	collect_0l_
	mov	-4(a3),a0
	sub	$4,a3
#ifdef PROFILE
	jmp	profile_r
#else
	ret
#endif

collect_2:
#ifdef PROFILE
	movl	$garbage_collector_name,a2
	call	profile_s
#endif
	mov	a0,K6_0(a3)
	mov	a1,4(a3)
	add	$8,a3
	call	collect_0_
	mov	-4(a3),a1
	mov	-8(a3),a0
	sub	$8,a3
#ifdef PROFILE
	jmp	profile_r
#else
	ret
#endif

collect_1:
#ifdef PROFILE
	movl	$garbage_collector_name,a2
	call	profile_s
#endif
	mov	a0,K6_0(a3)
	add	$4,a3
	call	collect_0_
	mov	-4(a3),a0
	sub	$4,a3
#ifdef PROFILE
	jmp	profile_r
#else
	ret
#endif

#ifdef PROFILE
collect_0:
	movl	$garbage_collector_name,a2
	call	profile_s
	call	collect_0_
	jmp	profile_r
collect_0l:
	pushl	a2
	movl	$garbage_collector_name,a2
	call	profile_s
	popl	a2
	call	collect_0l_
	jmp	profile_r	
#endif

#ifndef PROFILE
collect_0:
#endif
collect_0_:
	movl	a4,a2
#ifndef PROFILE
collect_0l:
#endif
collect_0l_:
	push	d0
	push	d1

	add	$32,a2
	sub	a4,a2
	shr	$2,a2
	mov	a2,n_allocated_words

#ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	je	no_mark3
#endif

#ifdef MARK_GC
	movl	bit_counter,a2
	testl	a2,a2
	je	no_scan

	xorl	d1,d1
	pushl	a3

	movl	n_allocated_words,a3
	movl	bit_vector_p,a0

scan_bits:
	cmpl	(a0),d1
	je	zero_bits
	movl	d1,(a0)
	addl	$4,a0
	subl	$1,a2
	jne	scan_bits

	jmp	end_scan

zero_bits:
	lea	4(a0),a1
	addl	$4,a0
	subl	$1,a2
	jne	skip_zero_bits_lp1
	jmp	end_bits

skip_zero_bits_lp:
	testl	d0,d0
	jne	end_zero_bits
skip_zero_bits_lp1:
	movl	(a0),d0
	addl	$4,a0
	subl	$1,a2
	jne	skip_zero_bits_lp

	testl	d0,d0
	je	end_bits
	movl	d1,-4(a0)
	movl	a0,d0
	subl	a1,d0
	jmp	end_bits2

end_zero_bits:
	movl	a0,d0
	subl	a1,d0
	shll	$3,d0
	addl	d0,n_free_words_after_mark
	movl	d1,-4(a0)

	cmpl	a3,d0
	jb	scan_bits

found_free_memory:
	movl	a2,bit_counter
	movl	a0,bit_vector_p

	lea	-4(a1),a2
	subl	heap_vector,a2
	shll	$5,a2
	movl	heap_p3,a4
	addl	a2,a4

	lea	(a4,d0,4),a2
	movl	a2,heap_end_after_gc
	subl	$32,a2
	movl	a2,end_heap

	popl	a3
	popl	d1
	popl	d0
	ret

end_bits:
	movl	a0,d0
	subl	a1,d0
	addl	$4,d0
end_bits2:
	shll	$3,d0
	addl	d0,n_free_words_after_mark
	cmpl	a3,d0
	jae	found_free_memory

end_scan:
	popl	a3
	movl	a2,bit_counter

no_scan:
#endif

#ifdef MARK_AND_COPY_GC
no_mark3:
#endif

	movsbl	garbage_collect_flag,d0
	test	d0,d0
	jle	collect

	subl	$2,d0
	movb	d0b,garbage_collect_flag

	movl	extra_heap_size,d1
	cmpl	d1,a2
	ja	collect

	movl	extra_heap,a4
	lea	(a4,d1,4),d1
	movl	d1,heap_end_after_gc
	subl	$32,d1
	movl	d1,end_heap

	pop	d1
	pop	d0
	ret

collect:
	call	add_execute_time

	testl	$4,@flags
	je	no_print_stack_sizes

	push	$garbage_collect_string_1
	call	@ew_print_string
	add	$4,sp

	mov	a3,d0
	sub	stack_p,d0
	push	d0
	call	@ew_print_int
	add	$4,sp

	push	$garbage_collect_string_2
	call	@ew_print_string
	add	$4,sp

	mov	halt_sp,d0
	sub	sp,d0
	push	d0
	call	@ew_print_int
	add	$4,sp

	push	$garbage_collect_string_3
	call	@ew_print_string
	add	$4,sp

no_print_stack_sizes:
	mov	stack_p,d0
	add	@ab_stack_size,d0
	cmp	d0,a3
	ja	stack_overflow

#ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	jne	compacting_collector
#else
# ifdef MARK_GC
	jmp	compacting_collector
# endif
#endif

#if defined (MARK_AND_COPY_GC) || !defined (MARK_GC)
	cmpb	$0,garbage_collect_flag
	jne	compacting_collector

	mov	heap_copied_vector,a2

	cmpl	$0,heap_end_after_copy_gc
	je	zero_all

	movl	a4,d0
	subl	heap_p1,d0
	addl	$63*4,d0
	shr	$8,d0
	call	zero_bit_vector

	movl	heap_end_after_copy_gc,a1
	subl	heap_p1,a1
	shr	$6,a1
	andl	$-4,a1

	movl	heap_copied_vector,a2
	movl	heap_copied_vector_size,d0
	addl	a1,a2
	subl	a1,d0
	shr	$2,d0

	movl	$0,heap_end_after_copy_gc

	call	zero_bit_vector
	jmp	end_zero_bit_vector

zero_all:
	mov	heap_copied_vector_size,d0
	shr	$2,d0
	call	zero_bit_vector

end_zero_bit_vector:

#include "icopy.s"

#ifdef WRITE_HEAP
	movl	a3,heap2_begin_and_end
#endif

	mov	a3,a2
	sub	a4,a2
	shr	$2,a2

#ifdef MEASURE_GC
	addl	a2,total_gc_bytes_lo
	jnc	no_total_gc_bytes_carry1
	incl	total_gc_bytes_hi
no_total_gc_bytes_carry1:
#endif

	pop	a3

	call	add_garbage_collect_time

	subl	n_allocated_words,a2
	jc	switch_to_mark_scan

	lea	(a2,a2,4),d0
	shl	$5,d0
	movl	@heap_size,d1
	mov	d1,a0
	shl	$2,d1
	add	a0,d1
	add	d1,d1
	add	a0,d1
	cmp	d1,d0
	jnc	no_mark_scan
/	jmp	no_mark_scan

switch_to_mark_scan:
	movl	heap_size_33,d0
	shl	$5,d0
	movl	heap_p,d1

	movl	heap_p1,a0
	cmpl	heap_p2,a0
	jc	vector_at_begin
	
vector_at_end:
	movl	d1,heap_p3
	add	d0,d1
	movl	d1,heap_vector
	
	movl	heap_p1,d0
	movl	d0,extra_heap
	subl	d0,d1
	shr	$2,d1
	movl	d1,extra_heap_size
	jmp	switch_to_mark_scan_2

vector_at_begin:
	movl	d1,heap_vector
	addl	@heap_size,d1
	subl	d0,d1
	movl	d1,heap_p3
	
	movl	d1,extra_heap
	movl	heap_p2,a0
	subl	d1,a0
	shr	$2,a0
	movl	a0,extra_heap_size

switch_to_mark_scan_2:
	movl	@heap_size,d0
	shr	$3,d0
	sub	a2,d0
	shl	$2,d0
	
	movb	$1,garbage_collect_flag
	
	test	a2,a2
	jns	end_garbage_collect
	
	movb	$-1,garbage_collect_flag
	
	movl	extra_heap_size,d1
	cmpl	n_allocated_words,d1
	jb	out_of_memory_4

	movl	extra_heap,a4
	shl	$2,d1
	addl	a4,d1
	movl	d1,heap_end_after_gc
#ifdef WRITE_HEAP
	movl	a4,heap_end_write_heap
#endif
	subl	$32,d1
	movl	d1,end_heap
#ifdef WRITE_HEAP
	movl	$1,d3_flag_write_heap
	jmp	end_garbage_collect_
#else
	jmp	end_garbage_collect
#endif
no_mark_scan:
/ exchange the semi_spaces
	mov	heap_p1,d0
	mov	heap_p2,d1
	mov	d0,heap_p2
	mov	d1,heap_p1

	mov	heap_size_129,d0
	shl	$6-2,d0

# ifdef MUNMAP
	mov	heap_p2,d1
	lea	(d1,d0,4),a0
	add	$4095,d1
	andl	$-4096,d1
	andl	$-4096,a0
	sub	d1,a0
	jbe	no_pages
	push	d0

	push	a0
	push	d1
	call	_munmap
	add	$8,sp

	pop	d0
no_pages:
# endif

# ifdef ADJUST_HEAP_SIZE
	movl	d0,d1
# endif
	sub	a2,d0

# ifdef ADJUST_HEAP_SIZE
	movl	d0,a0
	imull	@heap_size_multiple
	shrd	$9,a1,d0
	shr	$9,a1
	jne	no_small_heap1

	cmpl	$(MINIMUM_HEAP_SIZE_2),d0
	jae	not_too_small1
	movl	$(MINIMUM_HEAP_SIZE_2),d0
not_too_small1:
	subl	d0,d1
	jb	no_small_heap1

	shl	$2,d1
	movl	heap_end_after_gc,a2
	subl	d1,end_heap
	movl	a2,heap_end_after_copy_gc
	subl	d1,a2
	movl	a2,heap_end_after_gc

no_small_heap1:
	movl	a0,d0
# endif

	shl	$2,d0
#endif

end_garbage_collect:
#ifdef WRITE_HEAP
	movl	a4,heap_end_write_heap
	movl	$0,d3_flag_write_heap
end_garbage_collect_:
#endif

	pushl	d0

	testl	$2,@flags
	je	no_heap_use_message

	pushl	d0
	
	push	$heap_use_after_gc_string_1
	call	@ew_print_string
	add	$4,sp
	
	call	@ew_print_int
	add	$4,sp
	
	push	$heap_use_after_gc_string_2
	call	@ew_print_string
	add	$4,sp

no_heap_use_message:

#ifdef FINALIZERS
	call	call_finalizers
#endif

	popl	d0

#ifdef WRITE_HEAP
	/* Check whether memory profiling is on or off */
	testb	$32,@flags
	je	no_write_heap

	cmpl	@min_write_heap_size,d0
	jb	no_write_heap

	pushl	a0
	pushl 	a1
	pushl	a2
	pushl	a3
	pushl	a4
	
	subl	$64,sp

	movl	d3_flag_write_heap,d0
	test	d0,d0
	jne	copy_to_compact_with_alloc_in_extra_heap	
	
	movsbl	garbage_collect_flag,d0
	
	movl	heap2_begin_and_end,a0
	movl	heap2_begin_and_end+4,a1

	movl	$heap_p1,d1
	
	testl	d0,d0
	je	gc0
	
	movl	$heap_p2,d1
	jg	gc1

	movl	$heap_p3,d1
	xor	a0,a0
	xor	a1,a1
	
gc0:
gc1:
	movl	(d1),d1
	
	/* fill record */

	movl	sp,d0
	
	movl	d1,0(d0)
	movl	a4,4(d0)			// klop dit?
	
	movl	a0,8(d0)			// heap2_begin
	movl	a1,12(d0)			// heap2_end
	
	movl	stack_p,d1
	movl	d1,16(d0)			// stack_begin

	movl	a3,20(d0)			// stack_end
	movl	$0,24(d0)			// text_begin
	movl	$0,28(d0)			// data_begin
	
	movl	$small_integers,32(d0)	// small_integers
	movl	$static_characters,36(d0)	// small_characters
	
	movl	$INT+2,40(d0)		// INT-descP
	movl	$CHAR+2,44(d0)		// CHAR-descP
	movl	$REAL+2,48(d0)		// REAL-descP
	movl	$BOOL+2,52(d0)		// BOOL-descP
	movl	$__STRING__+2,56(d0)	// STRING-descP
	movl	$__ARRAY__+2,60(d0)		// ARRAY-descP
	
	pushl	d0
	call	@write_heap
	
	addl	$68,sp
	
	popl	a4
	popl	a3
	popl	a2
	popl	a1
	popl	a0
no_write_heap:
	
#endif

	pop	d1
	pop	d0
	ret

#ifdef FINALIZERS
call_finalizers:
	movl	free_finalizer_list,d0

call_finalizers_lp:
	cmpl	$__Nil-4,d0
	je	end_call_finalizers
	pushl	4(d0)
	movl	8(d0),d1
	pushl	4(d1)
	call	*(d1)
	addl	$4,sp
	pop	d0
	jmp	call_finalizers_lp
end_call_finalizers:

	movl	$__Nil-4,free_finalizer_list
	ret
#endif

#ifdef WRITE_HEAP
copy_to_compact_with_alloc_in_extra_heap:
	movl	heap2_begin_and_end,a0
	movl	heap2_begin_and_end+4,a1
	movl	$heap_p2,d1
	jmp	gc1
#endif

out_of_memory_4:
	call	add_garbage_collect_time
	
	mov	$out_of_memory_string_4,a2
	jmp	print_error

zero_bit_vector:
	xorl	a1,a1
	testb	$1,d0b
	je	zero_bits1_1
	mov	a1,(a2)
	add	$4,a2
zero_bits1_1:
	shr	$1,d0

	mov	d0,d1
	shr	$1,d0
	testb	$1,d1b
	je	zero_bits1_5

	sub	$8,a2
	jmp	zero_bits1_2

zero_bits1_4:
	mov	a1,(a2)
	mov	a1,4(a2)
zero_bits1_2:
	mov	a1,8(a2)
	mov	a1,12(a2)
	add	$16,a2
zero_bits1_5:
	sub	$1,d0
	jae	zero_bits1_4
	ret

reorder:
	pushl	a3
	pushl	a2

	movl	d0,a2
	shl	$2,a2
	movl	d1,a3
	shl	$2,a3
	addl	a3,a0
	subl	a2,a1

	pushl	a3
	pushl	a2
	pushl	d1
	pushl	d0
	jmp	st_reorder_lp

reorder_lp:
	movl	(a0),a2
	movl	-4(a1),a3
	movl	a2,-4(a1)
	subl	$4,a1
	movl	a3,(a0)
	addl	$4,a0
	
	dec	d0
	jne	next_b_in_element
	movl	(sp),d0
	addl	12(sp),a0
next_b_in_element:
	dec	d1
	jne	next_a_in_element
	movl	4(sp),d1
	subl	8(sp),a1
next_a_in_element:
st_reorder_lp:
	cmpl	a0,a1
	ja	reorder_lp

	popl	d0
	popl	d1
	addl	$8,sp
	popl	a2
	popl	a3
	ret

/
/	the sliding compacting garbage collector
/

compacting_collector:
/ zero all mark bits

	movl	heap_p3,d0
	negl	d0
	movl	d0,neg_heap_p3

	movl	a3,stack_top

	movl	heap_vector,a4

#ifdef MARK_GC
# ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	je	no_mark4
# endif
	cmpl	$0,zero_bits_before_mark
	je	no_zero_bits

	movl	$0,zero_bits_before_mark

# ifdef MARK_AND_COPY_GC
no_mark4:
# endif
#endif

	movl	a4,a2
	movl	heap_size_33,d0
	addl	$3,d0
	shr	$2,d0

	xorl	d1,d1

	testb	$1,d0b
	je	zero_bits_1
	movl	d1,(a2)
	addl	$4,a2
zero_bits_1:
	movl	d0,a0
	shr	$2,d0

	testb	$2,a0b
	je	zero_bits_5

	subl	$8,a2
	jmp	zero_bits_2

zero_bits_4:
	movl	d1,(a2)
	movl	d1,4(a2)
zero_bits_2:
	movl	d1,8(a2)
	movl	d1,12(a2)
	addl	$16,a2
zero_bits_5:
	subl	$1,d0
	jnc	zero_bits_4

#ifdef MARK_GC
# ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	je	no_mark5
# endif
no_zero_bits:
	movl	n_last_heap_free_bytes,d0
	movl	n_free_words_after_mark,d1

#if 1
	shrl	$2,d0
#else
	shll	$2,d1
#endif

	movl	d1,a2
	shll	$3,a2
	addl	d1,a2
	shrl	$2,a2

	cmpl	a2,d0
	jg	compact_gc

# ifdef ADJUST_HEAP_SIZE
	movl	bit_vector_size,d1
	shl	$2,d1

	subl	d1,d0
	negl	d0

	imull	@heap_size_multiple
	shrd	$7,a1,d0
	shr	$7,a1
	jne	no_smaller_heap
	
	cmpl	d1,d0
	jae	no_smaller_heap
	
	cmpl	$(MINIMUM_HEAP_SIZE),d1
	jbe	no_smaller_heap
	
	jmp	compact_gc
no_smaller_heap:
# endif

#include "imark.s"

compact_gc:
	movl	$1,zero_bits_before_mark
	movl	$0,n_last_heap_free_bytes
	movl	$1000,n_free_words_after_mark
# ifdef MARK_AND_COPY_GC
no_mark5:
# endif
#endif

#include "icompact.s"

	movl	stack_top,a3

	movl	heap_size_33,d1
	shl	$5,d1
	addl	heap_p3,d1

	movl	d1,heap_end_after_gc
	lea	-32(d1),d0
	movl	d0,end_heap

	subl	a4,d1
	shr	$2,d1

	subl	n_allocated_words,d1
	jc	out_of_memory_4

	cmpl	$107374182,d1
	jae	not_out_of_memory
	movl	d1,d0
	shl	$2,d0
	addl	d1,d0
	shl	$3,d0
	cmpl	@heap_size,d0
	jc	out_of_memory_4
not_out_of_memory:

#if defined (MARK_GC) || defined (COMPACT_GC_ONLY)
# if defined (MARK_GC) && defined (ADJUST_HEAP_SIZE)
#  ifdef MARK_AND_COPY_GC
 	testb	$64,@flags
	je	no_mark_6
#  endif

	movl	neg_heap_p3,d0
	addl	a4,d0
	movl	n_allocated_words,d1
	lea	(d0,d1,4),d0

	movl	heap_size_33,d1
	shl	$5,d1
	
	imull	@heap_size_multiple
	shrd	$8,a1,d0
	shr	$8,a1
	jne	no_small_heap2

	andl	$-4,d0
	
	cmpl	$(MINIMUM_HEAP_SIZE),d0
	jae	not_too_small2
	movl	$(MINIMUM_HEAP_SIZE),d0
not_too_small2:
	movl	d1,a0
	subl	d0,a0
	jb	no_small_heap2
	
	subl	a0,heap_end_after_gc
	subl	a0,end_heap	

	movl	d0,d1

no_small_heap2:
	shr	$2,d1
	movl	d1,bit_vector_size

#  ifdef MARK_AND_COPY_GC
no_mark_6:
#  endif
# endif
	jmp	no_copy_garbage_collection
#else
	shl	$2,d0
	movl	@heap_size,a0
	shl	$5,a0
	subl	@heap_size,a0
	cmpl	a0,d0
	jle	no_copy_garbage_collection

	movl	heap_p,d0
	movl	d0,heap_p1

	movl	heap_size_129,d1
	shl	$6,d1
	addl	d1,d0
	movl	d0,heap_copied_vector
	movl	d0,heap_end_after_gc
	lea	-32(d0),d1
	movl	d1,end_heap
	movl	heap_copied_vector_size,d1
	addl	d0,d1
	movl	d1,heap_p2

	movl	heap_p3,d0
	cmpl	heap_vector,d0
	jle	vector_at_end_2

	movl	heap_vector,d1
	movl	d1,extra_heap
	subl	d1,d0
	shr	$2,d0
	movl	d0,extra_heap_size

	movb	$2,garbage_collect_flag
	jmp	no_copy_garbage_collection

vector_at_end_2:
	movb	$0,garbage_collect_flag
#endif

no_copy_garbage_collection:
#ifdef MEASURE_GC
	call	add_compact_garbage_collect_time

	movl	a4,d0
	subl	heap_p3,d0

	addl	d0,total_compact_gc_bytes_lo
	jnc	no_total_compact_gc_bytes_carry
	incl	total_compact_gc_bytes_hi
no_total_compact_gc_bytes_carry:
#else
	call	add_garbage_collect_time
#endif

	movl	a4,d0
	subl	heap_p3,d0
	movl	n_allocated_words,d1
	lea	(d0,d1,4),d0
	jmp	end_garbage_collect

#ifdef _WINDOWS_
	.globl	_clean_exception_handler?4
_clean_exception_handler?4:
	movl	4(%esp),%eax
	movl	(%eax),%eax
	cmpl	$0xc00000fd,(%eax)	//	EXCEPTION_STACK_OVERFLOW
	je  	stack_overflow_exception

	cmpl	$0x80000001,(%eax)	//	EXCEPTION_GUARD_PAGE
	je  	guard_page_or_access_violation_exception

	cmpl	$0xc0000005,(%eax)	//	EXCEPTION_ACCESS_VIOLATION
	je  	guard_page_or_access_violation_exception

no_stack_overflow_exception:
	movl	$0,%eax				//	EXCEPTION_CONTINUE_SEARCH
	ret 	$4

guard_page_or_access_violation_exception:
	movl	0x18(%eax),%eax
	andl	$-4096,%eax
	cmpl	%eax,a_stack_guard_page
	jne 	no_stack_overflow_exception
	
	cmpl	$0,a_stack_guard_page
	je  	no_stack_overflow_exception

stack_overflow_exception:
	movl	4(%esp),%eax
	movl	4(%eax),%eax
	movl	$stack_overflow,0xb8(%eax)

	movl	$-1,%eax			//	EXCEPTION_CONTINUE_EXECUTION
	ret 	$4
#endif

stack_overflow:
	call	add_execute_time

	mov	$stack_overflow_string,a2
	jmp	print_error

@IO_error:
	addl	$4,sp
	
	pushl	$IO_error_string
	call	@ew_print_string
	addl	$4,sp
	
	call	@ew_print_string
	addl	$4,sp

	pushl	$new_line_string
	call	@ew_print_string
	addl	$4,sp

	jmp	halt

print_error:
	push	a2
	call	@ew_print_string
	add	$4,sp

halt:
	mov	halt_sp,sp

#ifdef PROFILE
	call	write_profile_stack
#endif

#ifdef _WINDOWS_
# if 0
	testb	$8,@flags
	jne	exit
	testb	$16,@flags
	je	exit
	call	@wait_for_key_press
# endif
#endif

	movl	$1,@execution_aborted

#ifdef _WINDOWS_
	cmpl	$0,dll_initisialised
	je	exit

	cmpl	$0,@return_code
	jne	return_code_set
	movl	$-1,@return_code
return_code_set:
	pushl	@return_code
	call	_ExitProcess?4
	jmp	return_code_set
#else
	jmp	exit
#endif

e__system__eaind:
__eaind:
eval_fill:
	mov	a0,K6_0(a3)
	add	$4,a3
	mov	a1,a0
	call	*(a1)
	mov	a0,a1
	mov	-4(a3),a0
	sub	$4,a3
	
	mov	(a1),a2
	mov	a2,(a0)
	mov	4(a1),a2
	mov	a2,4(a0)
	mov	8(a1),a2
	mov	a2,8(a0)
	ret

	align	(2)
	movl	$e__system__eaind,d0
	jmp	*d0
	.space	5
	.long	e__system__dind
	.long	-2
e__system__nind:
__indirection:
	mov	4(a0),a1
	mov	(a1),d0
	testb	$2,d0b
#ifdef MARK_GC
	je	eval_fill2
#else
	je	__cycle__in__spine
#endif
	mov	d0,(a0)
	mov	4(a1),a2
	mov	a2,4(a0)
	mov	8(a1),a2
	mov	a2,8(a0)
	ret

#ifdef MARK_GC
eval_fill2:
	movl	$__cycle__in__spine,(a0)
	movl	a0,K6_0(a3)
# ifdef MARK_AND_COPY_GC
	testb	$64,@flags
	je	__cycle__in__spine	
# endif
	addl	$4,a3
	movl	a1,a0
	call	*d0
	movl	a0,a1
	movl	-4(a3),a0
	subl	$4,a3
	
	mov	(a1),a2
	mov	a2,(a0)
	mov	4(a1),a2
	mov	a2,4(a0)
	mov	8(a1),a2
	mov	a2,8(a0)
	ret
#endif

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_0:
	movl	$__indirection,(a1)
	mov	a0,4(a1)
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_1:
	movl	$__indirection,(a1)
	mov	4(a1),d0
	mov	a0,4(a1)
	mov	d0,a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_2:
	movl	$__indirection,(a1)
	mov	4(a1),d0
	mov	a0,4(a1)
	mov	a0,K6_0(a3)
	add	$4,a3
	mov	8(a1),a0
	mov	d0,a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_3:
	movl	$__indirection,(a1)
	mov	4(a1),d0
	mov	a0,4(a1)
	mov	a0,K6_0(a3)
	mov	12(a1),d1
	mov	d1,4(a3)
	add	$8,a3
	mov	8(a1),a0
	mov	d0,a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_4:
	movl	$__indirection,(a1)
	mov	4(a1),d0
	mov	a0,4(a1)
	mov	a0,K6_0(a3)
	mov	16(a1),d1
	mov	d1,4(a3)
	mov	12(a1),d1
	mov	d1,8(a3)
	add	$12,a3
	mov	8(a1),a0
	mov	d0,a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_5:
	movl	$__indirection,(a1)
	mov	4(a1),d0
	mov	a0,K6_0(a3)
	mov	a0,4(a1)
	mov	20(a1),d1
	mov	d1,4(a3)
	mov	16(a1),d1
	mov	d1,8(a3)
	mov	12(a1),d1
	mov	d1,12(a3)
	add	$16,a3
	mov	8(a1),a0
	mov	d0,a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_6:
	movl	$__indirection,(a1)
	mov	4(a1),d0
	mov	a0,K6_0(a3)
	mov	a0,4(a1)
	mov	24(a1),d1
	mov	d1,4(a3)
	mov	20(a1),d1
	mov	d1,8(a3)
	mov	16(a1),d1
	mov	d1,12(a3)
	mov	12(a1),d1
	mov	d1,16(a3)
	add	$20,a3
	mov	8(a1),a0
	mov	d0,a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_7:
	mov	$0,d0
	mov	$20,d1
eval_upd_n:
	movl	$__indirection,(a1)
	push	4(a1)
	mov	a0,K6_0(a3)
	mov	a0,4(a1)
	add	d1,a1
	mov	8(a1),d1
	mov	d1,4(a3)
	mov	4(a1),d1
	mov	d1,8(a3)
	mov	(a1),d1
	mov	d1,12(a3)
	add	$16,a3

eval_upd_n_lp:
	mov	-4(a1),d1
	sub	$4,a1
	mov	d1,K6_0(a3)
	add	$4,a3
	sub	$1,d0
	jnc	eval_upd_n_lp

	mov	-4(a1),d1
	mov	d1,K6_0(a3)
	add	$4,a3
	mov	-8(a1),a0
	pop	a1
	jmp	*a2

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_8:
	mov	$1,d0
	mov	$24,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_9:
	mov	$2,d0
	mov	$28,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_10:
	mov	$3,d0
	mov	$32,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_11:
	mov	$4,d0
	mov	$36,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_12:
	mov	$5,d0
	mov	$40,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_13:
	mov	$6,d0
	mov	$44,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_14:
	mov	$7,d0
	mov	$48,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_15:
	mov	$8,d0
	mov	$52,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_16:
	mov	$9,d0
	mov	$56,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_17:
	mov	$10,d0
	mov	$60,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_18:
	mov	$11,d0
	mov	$64,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_19:
	mov	$12,d0
	mov	$68,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_20:
	mov	$13,d0
	mov	$72,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_21:
	mov	$14,d0
	mov	$76,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_22:
	mov	$15,d0
	mov	$80,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_23:
	mov	$16,d0
	mov	$84,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_24:
	mov	$17,d0
	mov	$88,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_25:
	mov	$18,d0
	mov	$92,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_26:
	mov	$19,d0
	mov	$96,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_27:
	mov	$20,d0
	mov	$100,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_28:
	mov	$21,d0
	mov	$104,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_29:
	mov	$22,d0
	mov	$108,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_30:
	mov	$23,d0
	mov	$112,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_31:
	mov	$24,d0
	mov	$116,d1
	jmp	eval_upd_n

#ifdef PROFILE
	call	profile_n
	movl	d0,a2
#endif
eval_upd_32:
	mov	$25,d0
	mov	$120,d1
	jmp	eval_upd_n

/
/	STRINGS
/

	section	(catAC)
catAC:
	mov	4(a0),a2
	add	4(a1),a2
	add	$8+3,a2
	shr	$2,a2

	lea	-32(a4,a2,4),a2
	cmpl	end_heap,a2
	jae	gc_3
gc_r_3:
	mov	4(a0),d0
	mov	4(a1),d1
	add	$8,a0
	add	$8,a1

/ fill_node

	push	a4
	movl	$__STRING__+2,(a4)

/ store length

	mov	d0,a2
	add	d1,a2
	mov	a2,4(a4)
	add	$8,a4

/ copy string 1

	lea	3(d1),a2
	shr	$2,a2
	add	a4,d1

	xchg	a2,%ecx
	xchg	a1,%esi
	cld
	rep
	movsl
	mov	a1,%esi
	mov	a2,%ecx

	mov	d1,a4

/ copy_string 2

cat_string_6:
	mov	d0,a2
	shr	$2,a2
	je	cat_string_9

cat_string_7:
	mov	(a0),d1
	add	$4,a0
	mov	d1,(a4)
	add	$4,a4
	dec	a2
	jne	cat_string_7
	
cat_string_9:
	testb	$2,d0b
	je	cat_string_10
	movw	(a0),d1w
	add	$2,a0
	movw	d1w,(a4)
	add	$2,a4
cat_string_10:
	testb	$1,d0b
	je	cat_string_11
	movb	(a0),d1b
	movb	d1b,(a4)
	inc	a4
cat_string_11:

	pop	a0
/ align heap pointer
	add	$3,a4
	andl	$-4,a4
	ret
	
gc_3:	call	collect_2l
	jmp	gc_r_3

empty_string:
	movl	$zero_length_string,a0
	ret

	section	(sliceAC)
sliceAC:
	mov	4(a0),a2
	test	d1,d1
	jns	slice_string_1
	xorl	d1,d1
slice_string_1:
	cmp	a2,d1
	jge	empty_string
	cmp	d1,d0
	jl	empty_string
	inc	d0
	cmp	a2,d0
	jle	slice_string_2
	mov	a2,d0
slice_string_2:
	sub	d1,d0

	lea	8+3(d0),a2
	shr	$2,a2

	lea	-32(a4,a2,4),a1
	cmpl	end_heap,a1
	jae	gc_4
r_gc_4:
	sub	$2,a2
	lea	8(a0,d1),a1

	movl	$__STRING__+2,(a4)
	mov	d0,4(a4)

/ copy part of string
	mov	a2,%ecx
	movl	a4,a2
	add	$8,a4

	xchg	a1,%esi
	cld
	rep
	movsl
	mov	a1,%esi
	mov	a2,a0
	ret

gc_4:
	movl	a1,a2
	call	collect_1l
	lea	8+3(d0),a2
	shr	$2,a2
	jmp	r_gc_4

	section	(updateAC)
updateAC:
	mov	4(a0),a2
	cmp	a2,d1
	jae	update_string_error

	add	$8+3,a2
	shr	$2,a2

	lea	-32(a4,a2,4),a2
	cmpl	end_heap,a2
	jae	gc_5
r_gc_5:
	mov	4(a0),a2
	add	$3,a2
	shr	$2,a2

	mov	a0,a1
	push	a4
	movl	$__STRING__+2,(a4)
	mov	4(a1),a0
	add	$8,a1
	mov	a0,4(a4)
	add	$8,a4

	add	a4,d1

	mov	a2,%ecx
	xchg	a1,%esi
	cld
	rep
	movsl
	mov	a1,%esi

	movb	d0b,(d1)
	pop	a0
	ret

gc_5:	call	collect_1l
	jmp	r_gc_5

update_string_error:
	movl	$high_index_string,a2
	test	d0,d0
	jns	update_string_error_2
	movl	$low_index_string,a2
update_string_error_2:
	jmp	print_error

	section	(eqAC)
eqAC:
	mov	4(a0),d0
	cmp	4(a1),d0
	jne	equal_string_ne
	add	$8,a0
	add	$8,a1
	mov	d0,d1
	andl	$3,d1
	shr	$2,d0
	je	equal_string_b
equal_string_1:
	mov	(a0),a2
	cmp	(a1),a2
	jne	equal_string_ne
	add	$4,a0
	add	$4,a1
	dec	d0
	jne	equal_string_1
equal_string_b:
	testb	$2,d1b
	je	equal_string_2
	movw	(a0),d0w
	cmpw	(a1),d0w
	jne	equal_string_ne
	add	$2,a0
	add	$2,a1
equal_string_2:
	testb	$1,d1b
	je	equal_string_eq
	movb	(a0),d1b
	cmpb	(a1),d1b
	jne	equal_string_ne
equal_string_eq:
	mov	$1,d0
	ret
equal_string_ne:
	xorl	d0,d0
	ret

	section	(cmpAC)
cmpAC:
	mov	4(a0),d1
	mov	4(a1),a2
	add	$8,a0
	add	$8,a1
	cmp	d1,a2
	jb	cmp_string_less
	ja	cmp_string_more
	xorl	d0,d0
	jmp	cmp_string_chars
cmp_string_more:
	mov	$1,d0
	jmp	cmp_string_chars
cmp_string_less:
	mov	$-1,d0
	mov	a2,d1
	jmp	cmp_string_chars

cmp_string_1:
	mov	(a1),a2
	cmp	(a0),a2
	jne	cmp_string_ne4
	add	$4,a1
	add	$4,a0
cmp_string_chars:
	sub	$4,d1
	jnc	cmp_string_1
cmp_string_b:
	testb	$2,d1b
	je	cmp_string_2
	movb	(a1),%bh
	cmpb	(a0),%bh
	jne	cmp_string_ne
	movb	1(a1),%bh
	cmpb	1(a0),%bh
	jne	cmp_string_ne
	add	$2,a1
	add	$2,a0
cmp_string_2:
	testb	$1,d1b
	je	cmp_string_eq
	movb	(a1),d1b
	cmpb	(a0),d1b
	jne	cmp_string_ne
cmp_string_eq:
	ret
cmp_string_ne4:
	movb	(a1),d1b
	cmpb	(a0),d1b
	jne	cmp_string_ne
	movb	1(a1),d1b
	cmpb	1(a0),d1b
	jne	cmp_string_ne
	movb	2(a1),d1b
	cmpb	2(a0),d1b
	jne	cmp_string_ne
	movb	3(a1),d1b
	cmpb	3(a0),d1b
cmp_string_ne:
	ja	cmp_string_r1
	mov	$-1,d0
	ret
cmp_string_r1:
	mov	$1,d0
	ret

	section	(string_to_string_node)
string_to_string_node:
	movl	(a0),d0
	addl	$4,a0

	lea	3(d0),d1
	shr	$2,d1

	lea	-32+8(a4,d1,4),a2
	cmpl	end_heap,a2
	jae	string_to_string_node_gc

string_to_string_node_r:
	movl	$__STRING__+2,(a4)
	movl	d0,4(a4)
	movl	a4,a2
	addl	$8,a4
	jmp	string_to_string_node_4
	
string_to_string_node_2:
	movl	(a0),d0
	addl	$4,a0
	movl	d0,(a4)
	addl	$4,a4
string_to_string_node_4:
	subl	$1,d1
	jge	string_to_string_node_2

	movl	a2,a0
	ret

string_to_string_node_gc:
	push	a0
	call	collect_0l
	pop	a0
	jmp	string_to_string_node_r

	section	(int_array_to_node)
int_array_to_node:
	movl	-8(a0),d0

	lea	-32+12(a4,d0,4),a2
	cmpl	end_heap,a2
	jae	int_array_to_node_gc

int_array_to_node_r:
	movl	$__ARRAY__+2,(a4)
	movl	a0,a1
	movl	d0,4(a4)
	movl	a4,a0
	movl	$INT+2,8(a4)
	addl	$12,a4
	jmp	int_array_to_node_4
	
int_array_to_node_2:
	movl	(a1),d1
	addl	$4,a1
	movl	d1,(a4)
	addl	$4,a4
int_array_to_node_4:
	subl	$1,d0
	jge	int_array_to_node_2

	ret

int_array_to_node_gc:
	push	a0
	call	collect_0l
	pop	a0
	jmp	int_array_to_node_r

	section	(real_array_to_node)
real_array_to_node:
	movl	-8(a0),d0

	lea	-32+12+4(a4,d0,8),a2
	cmpl	end_heap,a2
	jae	real_array_to_node_gc

real_array_to_node_r:
	orl	$4,a4
	movl	a0,a1
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a4,a0
	movl	$REAL+2,8(a4)
	addl	$12,a4
	jmp	real_array_to_node_4
	
real_array_to_node_2:
	movl	(a1),d1
	movl	d1,(a4)
	movl	4(a1),a2
	addl	$8,a1
	movl	a2,4(a4)
	addl	$8,a4
real_array_to_node_4:
	subl	$1,d0
	jge	real_array_to_node_2

	ret

real_array_to_node_gc:
	push	a0
	call	collect_0l
	pop	a0
	jmp	real_array_to_node_r

	align	(2)
	.long	3
_c3:	jmp	__cycle__in__spine
	align	(2)

	.long	4
_c4:	jmp	__cycle__in__spine
	align	(2)
	.long	5
_c5:	jmp	__cycle__in__spine
	align	(2)
	.long	6
_c6:	jmp	__cycle__in__spine
	align	(2)
	.long	7
_c7:	jmp	__cycle__in__spine
	align	(2)
	.long	8
_c8:	jmp	__cycle__in__spine
	align	(2)
	.long	9
_c9:	jmp	__cycle__in__spine
	align	(2)
	.long	10
_c10:	jmp	__cycle__in__spine
	align	(2)
	.long	11
_c11:	jmp	__cycle__in__spine
	align	(2)
	.long	12
_c12:	jmp	__cycle__in__spine
	align	(2)
	.long	13
_c13:	jmp	__cycle__in__spine
	align	(2)
	.long	14
_c14:	jmp	__cycle__in__spine
	align	(2)
	.long	15
_c15:	jmp	__cycle__in__spine
	align	(2)
	.long	16
_c16:	jmp	__cycle__in__spine
	align	(2)
	.long	17
_c17:	jmp	__cycle__in__spine
	align	(2)
	.long	18
_c18:	jmp	__cycle__in__spine
	align	(2)
	.long	19
_c19:	jmp	__cycle__in__spine
	align	(2)
	.long	20
_c20:	jmp	__cycle__in__spine
	align	(2)
	.long	21
_c21:	jmp	__cycle__in__spine
	align	(2)
	.long	22
_c22:	jmp	__cycle__in__spine
	align	(2)
	.long	23
_c23:	jmp	__cycle__in__spine
	align	(2)
	.long	24
_c24:	jmp	__cycle__in__spine
	align	(2)
	.long	25
_c25:	jmp	__cycle__in__spine
	align	(2)
	.long	26
_c26:	jmp	__cycle__in__spine
	align	(2)
	.long	27
_c27:	jmp	__cycle__in__spine
	align	(2)
	.long	28
_c28:	jmp	__cycle__in__spine
	align	(2)
	.long	29
_c29:	jmp	__cycle__in__spine
	align	(2)
	.long	30
_c30:	jmp	__cycle__in__spine
	align	(2)
	.long	31
_c31:	jmp	__cycle__in__spine
	align	(2)
	.long	32
_c32:	jmp	__cycle__in__spine

/
/	ARRAYS
/

_create_arrayB:
	movl	d0,d1
	addl	$3,d0
	shr	$2,d0

	lea	-32+12(a4,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4574
	call	collect_0l
no_collect_4574:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d1,4(a4)
	movl	$BOOL+2,8(a4)
	lea	12(a4,d0,4),a4
	ret

_create_arrayC:
	movl	d0,d1
	addl	$3,d0
	shr	$2,d0

	lea	-32+8(a4,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4573
	call	collect_0l
no_collect_4573:
	movl	a4,a0
	movl	$__STRING__+2,(a4)
	movl	d1,4(a4)
	lea	8(a4,d0,4),a4
	ret

_create_arrayI:
	lea	-32+12(a4,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4572
	call	collect_0l
no_collect_4572:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	$INT+2,8(a4)
	lea	12(a4,d0,4),a4
	ret

_create_arrayR:
	lea	-32+12+4(a4,d0,8),a2
	cmpl	end_heap,a2
	jb	no_collect_4580
	call	collect_0l
no_collect_4580:
	orl	$4,a4
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	$REAL+2,8(a4)
	lea	12(a4,d0,8),a4
	ret

/ vier(sp): number of elements, (sp): element descriptor
/ d0: element size, d1: element a size a0:a_element-> a0: array

_create_r_array:
	movl	4(sp),a1

	pushl	d0

	shl	$2,a1
	lea	12-32(a4),a2
_sub_size_lp:
	addl	a1,a2
	subl	$1,d0
	jne	_sub_size_lp

	popl	d0
	
	cmpl	end_heap,a2
	jb	no_collect_4586
	call	collect_1l
no_collect_4586:
	movl	a0,a2

	pop	a0
	pop	a1

/ a1: number of elements, a0: element descriptor
/ d0: element size, d1: element a size a2:a_element

	movl	$__ARRAY__+2,(a4)
	movl	a1,4(a4)
	movl	a0,8(a4)

	movl	a4,a0
	addl	$12,a4

/ a1: number of elements, a0: array
/ d0: element size, d1: element a size a2:a_element

	test	d1,d1
	je	_create_r_array_0
	subl	$2,d1
	jc	_create_r_array_1
	je	_create_r_array_2
	subl	$2,d1
	jc	_create_r_array_3
	je	_create_r_array_4
	jmp	_create_r_array_5

_create_r_array_0:
	shl	$2,a1
	jmp	_st_fillr0_array
_fillr0_array:
	addl	a1,a4
_st_fillr0_array:
	subl	$1,d0
	jnc	_fillr0_array
	ret

_create_r_array_1:
	shl	$2,d0
	jmp	_st_fillr1_array
_fillr1_array:
	movl	a2,(a4)
	addl	d0,a4
_st_fillr1_array:
	subl	$1,a1
	jnc	_fillr1_array
	ret

_create_r_array_2:
	shl	$2,d0
	jmp	_st_fillr2_array
_fillr2_array:
	movl	a2,(a4)
	movl	a2,4(a4)
	addl	d0,a4
_st_fillr2_array:
	subl	$1,a1
	jnc	_fillr2_array
	ret

_create_r_array_3:
	shl	$2,d0
	jmp	_st_fillr3_array
_fillr3_array:
	movl	a2,(a4)
	movl	a2,4(a4)
	movl	a2,8(a4)
	addl	d0,a4
_st_fillr3_array:
	subl	$1,a1
	jnc	_fillr3_array
	ret

_create_r_array_4:
	shl	$2,d0
	jmp	_st_fillr4_array
_fillr4_array:
	movl	a2,(a4)
	movl	a2,4(a4)
	movl	a2,8(a4)
	movl	a2,12(a4)
	addl	d0,a4
_st_fillr4_array:
	subl	$1,a1
	jnc	_fillr4_array
	ret

_create_r_array_5:
	push	a0
	
	movl	d1,a0
	subl	$4,d0
	subl	d1,d0
	
	subl	$1,a0
	shl	$2,d0
	jmp	_st_fillr5_array
_fillr5_array:
	movl	a2,(a4)
	movl	a2,4(a4)
	movl	a2,8(a4)
	movl	a2,12(a4)
	addl	$16,a4

	movl	a0,d1
_copy_elem_5_lp:
	movl	a2,(a4)
	addl	$4,a4
	subl	$1,d1
	jnc	_copy_elem_5_lp
	
	addl	d0,a4
_st_fillr5_array:
	subl	$1,a1
	jnc	_fillr5_array
	
	pop	a0
	ret

create_arrayB:
	movl	d1,a1
	addl	$3,d1
	shr	$2,d1

	lea	-32+12(a4,d1,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4575

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4575:
	movl	d0,a2
	shl	$8,a2
	orl	a2,d0
	movl	d0,a2
	shl	$16,a2
	orl	a2,d0
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	a1,4(a4)
	movl	$BOOL+2,8(a4)
	addl	$12,a4
	jmp	create_arrayBCI

create_arrayC:
	movl	d1,a1
	addl	$3,d1
	shr	$2,d1

	lea	-32+8(a4,d1,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4578

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4578:
	movl	d0,a2
	shl	$8,a2
	orl	a2,d0
	movl	d0,a2
	shl	$16,a2
	orl	a2,d0
	movl	a4,a0
	movl	$__STRING__+2,(a4)
	movl	a1,4(a4)
	addl	$8,a4
	jmp	create_arrayBCI

create_arrayI:
	lea	-32+12(a4,d1,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4577

	call	collect_0l

no_collect_4577:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d1,4(a4)
	lea	0(,d1,4),a1
	movl	$INT+2,8(a4)
	addl	$12,a4
create_arrayBCI:
	mov	d1,a1
	shr	$1,d1
	testb	$1,a1b
	je	st_filli_array

	movl	d0,(a4)
	addl	$4,a4
	jmp	st_filli_array

filli_array:
	movl	d0,(a4)
	movl	d0,4(a4)
	addl	$8,a4
st_filli_array:
	subl	$1,d1
	jnc	filli_array

	ret

create_arrayR:
	fstl	-8(sp)

	lea	-32+12+4(a4,d0,8),a2

	movl	-8(sp),d1
	movl	-4(sp),a1

	cmpl	end_heap,a2
	jb	no_collect_4579
	pushl	a1
	call	collect_0l
	popl	a1
no_collect_4579:
	orl	$4,a4

	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	$REAL+2,8(a4)
	addl	$12,a4
	jmp	st_fillr_array
fillr_array:
	movl	d1,(a4)
	movl	a1,4(a4)
	addl	$8,a4
st_fillr_array:
	subl	$1,d0
	jnc	fillr_array

	ret

create_array:
	lea	-32+12(a4,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4576

	call	collect_1l

no_collect_4576:
	movl	a0,d1
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	$0,8(a4)
	addl	$12,a4
#if 0
	popl	a2
#endif
	jmp	fillr1_array

#if 0

/ in 4(sp): number of elements, (sp): element descriptor
/ d0: element size, d1: element a size -> a0: array

create_r_array:
	subl	$2,d0
	jc	create_r_array_1
	je	create_r_array_2
	subl	$2,d0
	jc	create_r_array_3
	je	create_r_array_4
	jmp	create_r_array_5

create_r_array_1:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	lea	-32+12(a4,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4581

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4581:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	popl	a2

	test	d1,d1
	je	r_array_1_b

	movl	-4(a3),d1
	subl	$4,a3
	jmp	fillr1_array

r_array_1_b:
	popl	d1

fillr1_array:
	movl	d0,a1
	shr	$1,d0
	testb	$1,a1b
	je	st_fillr1_array_1

	movl	d1,(a4)
	addl	$4,a4
	jmp	st_fillr1_array_1

fillr1_array_lp:
	movl	d1,(a4)
	movl	d1,4(a4)
	addl	$8,a4
st_fillr1_array_1:
	subl	$1,d0
	jnc	fillr1_array_lp

	jmp	*a2

create_r_array_2:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	lea	-32+12(a4,d0,8),a2
	cmpl	end_heap,a2
	jb	no_collect_4582

	pushl	a1
	call	collect_0
	popl	a1

no_collect_4582:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	popl	a1

	subl	$1,d1
	jc	r_array_2_bb
	je	r_array_2_ab
r_array_2_aa:
	movl	-4(a3),d1
	movl	-8(a3),a2
	subl	$8,a3
	jmp	st_fillr2_array
r_array_2_ab:
	movl	-4(a3),d1
	popl	a2
	subl	$4,a3
	jmp	st_fillr2_array
r_array_2_bb:
	popl	d1
	popl	a2
	jmp	st_fillr2_array

fillr2_array_1:
	movl	d1,(a4)
	movl	a2,4(a4)
	addl	$8,a4
st_fillr2_array:
	subl	$1,d0
	jnc	fillr2_array_1

	jmp	*a1

create_r_array_3:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	lea	-32+12(a4,d0,8),a2
	lea	(a2,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4583

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4583:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	popl	a1

	test	d1,d1
	je	r_array_3

	movl	d1,a2
	shl	$2,a2
	subl	a2,a3
	mov	a3,a2
	subl	$1,d1

copy_a_to_b_lp3:
	pushl	(a2)
	addl	$4,a2
	subl	$1,d1
	jnc	copy_a_to_b_lp3

r_array_3:
	movl	a0,K6_0(a3)

	popl	d1
	popl	a0
	popl	a2
	jmp	st_fillr3_array

fillr3_array_1:
	movl	d1,(a4)
	movl	a0,4(a4)
	movl	a2,8(a4)
	addl	$12,a4
st_fillr3_array:
	subl	$1,d0
	jnc	fillr3_array_1

	movl	K6_0(a3),a0
	jmp	*a1

create_r_array_4:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	movl	d0,a2
	shl	$4,a2
	lea	-32+12(a4,a2),a2
	cmpl	end_heap,a2
	jb	no_collect_4584

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4584:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	popl	a1

	test	d1,d1
	je	r_array_4

	movl	d1,a2
	shl	$2,a2
	subl	a2,a3
	movl	a3,a2
	subl	$1,d1

copy_a_to_b_lp4:
	pushl	(a2)
	addl	$4,a2
	subl	$1,d1
	jnc	copy_a_to_b_lp4

r_array_4:
	popl	d1
	movl	a0,K6_0(a3)
	movl	a1,4(a3)
	popl	a0
	popl	a1
	popl	a2
	jmp	st_fillr4_array

fillr4_array:
	movl	d1,(a4)
	movl	a0,4(a4)
	movl	a1,8(a4)
	movl	a2,12(a4)
	addl	$16,a4
st_fillr4_array:
	subl	$1,d0
	jnc	fillr4_array

	movl	4(a3),a1
	movl	K6_0(a3),a0
	jmp	*a1

create_r_array_5:
	pop	a1
	pop	a0

/ a0: number of elements, a1: element descriptor
/ d0: element size-4, d1: element a size

	movl	a0,a2
	shl	$4,a2
	lea	12-32(a4,a2),a2

	subl	$1,d0

	pushl	d0
sub_size_lp:
	lea	(a2,a0,4),a2
	subl	$1,d0
	jnc	sub_size_lp
	popl	d0

	cmpl	end_heap,a2
	jb	no_collect_4585

	pushl	a1
	pushl	a0
	call	collect_0l
	popl	a0
	popl	a1

no_collect_4585:
	movl	$__ARRAY__+2,(a4)
	movl	a0,4(a4)
	movl	a1,8(a4)
	popl	a1

	test	d1,d1
	je	r_array_5

	movl	d1,a2
	shl	$2,a2
	subl	a2,a3
	movl	a3,a2
	subl	$1,d1

copy_a_to_b_lp5:
	pushl	(a2)
	addl	$4,a2
	subl	$1,d1
	jnc	copy_a_to_b_lp5

r_array_5:
	movl	a4,K6_0(a3)
	movl	a1,4(a3)
	addl	$12,a4

	popl	d1
	popl	a1
	pushl	a3

	jmp	st_fillr5_array

fillr5_array_1:
	movl	d1,(a4)
	movl	a1,4(a4)

	lea	4(sp),a3
	pushl	d0

	movl	K6_0(a3),a2
	movl	a2,8(a4)

	movl	4(a3),a2
	addl	$8,a3
	movl	a2,12(a4)
	addl	$16,a4

copy_elem_lp5:
	movl	K6_0(a3),a2
	addl	$4,a3
	movl	a2,(a4)
	addl	$4,a4
	subl	$1,d0
	jnc	copy_elem_lp5

	popl	d0

st_fillr5_array:
	subl	$1,a0
	jnc	fillr5_array_1

	popl	a3
	shl	$2,d0

	movl	4(a3),a1
	addl	d0,sp

	movl	K6_0(a3),a0
	addl	$12,sp

	jmp	*a1

#else

/ in 4(sp): number of elements, (sp): element descriptor
/ d0: element size, d1: element a size -> a0: array

create_R_array:
	subl	$2,d0
	jc	create_R_array_1
	je	create_R_array_2
	subl	$2,d0
	jc	create_R_array_3
	je	create_R_array_4
	jmp	create_R_array_5

create_R_array_1:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	lea	-32+12(a4,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4581

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4581:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	test	d1,d1
	je	r_array_1_b

	movl	-4(a3),d1
	jmp	fillr1_array

r_array_1_b:
	movl	4(sp),d1

fillr1_array:
	movl	d0,a1
	shr	$1,d0
	testb	$1,a1b
	je	st_fillr1_array_1

	movl	d1,(a4)
	addl	$4,a4
	jmp	st_fillr1_array_1

fillr1_array_lp:
	movl	d1,(a4)
	movl	d1,4(a4)
	addl	$8,a4
st_fillr1_array_1:
	subl	$1,d0
	jnc	fillr1_array_lp

	ret

create_R_array_2:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	lea	-32+12(a4,d0,8),a2
	cmpl	end_heap,a2
	jb	no_collect_4582

	pushl	a1
	call	collect_0
	popl	a1

no_collect_4582:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	subl	$1,d1
	jc	r_array_2_bb
	je	r_array_2_ab
r_array_2_aa:
	movl	-4(a3),d1
	movl	-8(a3),a2
	jmp	st_fillr2_array
r_array_2_ab:
	movl	-4(a3),d1
	movl	4(sp),a2
	jmp	st_fillr2_array
r_array_2_bb:
	movl	4(sp),d1
	movl	8(sp),a2
	jmp	st_fillr2_array

fillr2_array_1:
	movl	d1,(a4)
	movl	a2,4(a4)
	addl	$8,a4
st_fillr2_array:
	subl	$1,d0
	jnc	fillr2_array_1

	ret

create_R_array_3:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	lea	-32+12(a4,d0,8),a2
	lea	(a2,d0,4),a2
	cmpl	end_heap,a2
	jb	no_collect_4583

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4583:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	popl	a1
	movl	sp,4(a3)

	test	d1,d1
	je	r_array_3

	movl	d1,a2
	shl	$2,a2
	negl	a2
	addl	a3,a2
	subl	$1,d1

copy_a_to_b_lp3:
	pushl	(a2)
	addl	$4,a2
	subl	$1,d1
	jnc	copy_a_to_b_lp3

r_array_3:
	movl	a0,K6_0(a3)

	movl	(sp),d1
	movl	4(sp),a0
	movl	8(sp),a2	

	movl	4(a3),sp
	jmp	st_fillr3_array

fillr3_array_1:
	movl	d1,(a4)
	movl	a0,4(a4)
	movl	a2,8(a4)
	addl	$12,a4
st_fillr3_array:
	subl	$1,d0
	jnc	fillr3_array_1

	movl	K6_0(a3),a0
	jmp	*a1

create_R_array_4:
	pop	a1
	pop	d0

/ d0: number of elements, a1: element descriptor
/ d1: element a size

	movl	d0,a2
	shl	$4,a2
	lea	-32+12(a4,a2),a2
	cmpl	end_heap,a2
	jb	no_collect_4584

	pushl	a1
	call	collect_0l
	popl	a1

no_collect_4584:
	movl	a4,a0
	movl	$__ARRAY__+2,(a4)
	movl	d0,4(a4)
	movl	a1,8(a4)
	addl	$12,a4

	popl	a1
	movl	sp,8(a3)

	test	d1,d1
	je	r_array_4

	movl	d1,a2
	shl	$2,a2
	negl	a2
	addl	a3,a2
	subl	$1,d1

copy_a_to_b_lp4:
	pushl	(a2)
	addl	$4,a2
	subl	$1,d1
	jnc	copy_a_to_b_lp4

r_array_4:
	movl	(sp),d1
	movl	a0,K6_0(a3)
	movl	a1,4(a3)
	movl	4(sp),a0
	movl	8(sp),a1
	movl	12(sp),a2
	
	movl	8(a3),sp
	jmp	st_fillr4_array

fillr4_array:
	movl	d1,(a4)
	movl	a0,4(a4)
	movl	a1,8(a4)
	movl	a2,12(a4)
	addl	$16,a4
st_fillr4_array:
	subl	$1,d0
	jnc	fillr4_array

	movl	4(a3),a1
	movl	K6_0(a3),a0
	jmp	*a1

create_R_array_5:
	pop	a1
	pop	a0

/ a0: number of elements, a1: element descriptor
/ d0: element size-4, d1: element a size

	movl	a0,a2
	shl	$4,a2
	lea	12-32(a4,a2),a2

	subl	$1,d0

	pushl	d0
sub_size_lp:
	lea	(a2,a0,4),a2
	subl	$1,d0
	jnc	sub_size_lp
	popl	d0

	cmpl	end_heap,a2
	jb	no_collect_4585

	pushl	a1
	pushl	a0
	call	collect_0l
	popl	a0
	popl	a1

no_collect_4585:
	movl	$__ARRAY__+2,(a4)
	movl	a0,4(a4)
	movl	a1,8(a4)

	popl	a1
	movl	sp,8(a3)
	
	test	d1,d1
	je	r_array_5

	movl	d1,a2
	shl	$2,a2
	negl	a2
	addl	a3,a2
	subl	$1,d1

copy_a_to_b_lp5:
	pushl	(a2)
	addl	$4,a2
	subl	$1,d1
	jnc	copy_a_to_b_lp5

r_array_5:
	movl	a4,K6_0(a3)
	movl	a1,4(a3)
	addl	$12,a4

	movl	(sp),d1
	movl	4(sp),a1

	pushl	a3
	jmp	st_fillr5_array

fillr5_array_1:
	movl	d1,(a4)
	movl	a1,4(a4)

	lea	12(sp),a3
	pushl	d0

	movl	K6_0(a3),a2
	movl	a2,8(a4)

	movl	4(a3),a2
	addl	$8,a3
	movl	a2,12(a4)
	addl	$16,a4

copy_elem_lp5:
	movl	K6_0(a3),a2
	addl	$4,a3
	movl	a2,(a4)
	addl	$4,a4
	subl	$1,d0
	jnc	copy_elem_lp5

	popl	d0

st_fillr5_array:
	subl	$1,a0
	jnc	fillr5_array_1

	popl	a3
	movl	8(a3),sp

	movl	4(a3),a1
	movl	K6_0(a3),a0
	jmp	*a1

#endif

#ifndef NEW_DESCRIPTORS

yet_args_needed:
/ for more than 4 arguments
	mov	(a1),d1
	movzwl	-2(d1),d0
	add	$3,d0
	lea	-32(a4,d0,4),a2
	cmpl	end_heap,a2
	jae	gc_1
gc_r_1:	sub	$3+1+4,d0
	push	d1
	push	a0
	mov	4(a1),d1
	mov	8(a1),a1
	mov	a4,a2
	mov	(a1),a0
	mov	a0,(a4)
	mov	4(a1),a0
	mov	a0,4(a4)
	mov	8(a1),a0
	mov	a0,8(a4)
	add	$12,a1
	add	$12,a4

cp_a:	mov	(a1),a0
	add	$4,a1
	mov	a0,(a4)
	add	$4,a4
	subl	$1,d0
	jge	cp_a

	pop	a0
	mov	a0,(a4)
	pop	d0
	add	$8,d0
	mov	d0,4(a4)
	lea	4(a4),a0
	mov	d1,8(a4)
	mov	a2,12(a4)
	add	$16,a4
	ret

gc_1:
	call	collect_2l
	jmp	gc_r_1

yet_args_needed_0:
	cmpl	end_heap,a4
	jae	gc_20
gc_r_20:	mov	a0,4(a4)
	mov	(a1),d0
	mov	a4,a0
	add	$8,d0
	mov	d0,(a4)
	add	$8,a4
	ret

gc_20:	call	collect_2
	jmp	gc_r_20

yet_args_needed_1:
	cmpl	end_heap,a4
	jae	gc_21
gc_r_21:	mov	a0,8(a4)
	mov	(a1),d0
	mov	a4,a0
	add	$8,d0
	mov	d0,(a4)
	mov	4(a1),d1
	mov	d1,4(a4)
	add	$12,a4
	ret

gc_21:	call	collect_2
	jmp	gc_r_21

yet_args_needed_2:
	cmpl	end_heap,a4
	jae	gc_22
gc_r_22:
	mov	(a1),d0
	mov	a0,4(a4)
	add	$8,d0
	mov	4(a1),a2
	mov	d0,8(a4)
	lea	8(a4),a0
	mov	a2,12(a4)
	mov	8(a1),a2
	mov	a2,(a4)
	mov	a4,16(a4)
	add	$20,a4
	ret

gc_22:	call	collect_2
	jmp	gc_r_22

yet_args_needed_3:
	cmpl	end_heap,a4
	jae	gc_23
gc_r_23:
	mov	(a1),d0
	mov	a0,8(a4)
	add	$8,d0
	mov	4(a1),a2
	mov	d0,12(a4)
	mov	8(a1),a1
	mov	a2,16(a4)
	mov	(a1),a2
	mov	a4,20(a4)
	mov	a2,(a4)
	mov	4(a1),a2
	lea	12(a4),a0
	mov	a2,4(a4)
	add	$24,a4
	ret

gc_23:	call	collect_2
	jmp	gc_r_23

yet_args_needed_4:
	cmpl	end_heap,a4
	jae	gc_24
gc_r_24:
	mov	(a1),d0
	mov	a0,12(a4)
	add	$8,d0
	mov	4(a1),a2
	mov	d0,16(a4)
	mov	8(a1),a1
	mov	a2,20(a4)
	mov	(a1),a2
	mov	a4,24(a4)
	mov	a2,(a4)
	mov	4(a1),a2
	lea	16(a4),a0
	mov	a2,4(a4)
	mov	8(a1),a2
	mov	a2,8(a4)
	add	$28,a4
	ret

gc_24:	call	collect_2
	jmp	gc_r_24

#endif

repl_args_b:
	test	d0,d0
	jle	repl_args_b_1

	dec	d0
	je	repl_args_b_4

	mov	8(a0),a1
	sub	$2,d1
	jne	repl_args_b_2

	mov	a1,K6_0(a3)
	add	$4,a3
	jmp	repl_args_b_4

repl_args_b_2:
	lea	(a1,d0,4),a1

repl_args_b_3:
	mov	-4(a1),a2
	sub	$4,a1
	mov	a2,K6_0(a3)
	add	$4,a3
	dec	d0
	jne	repl_args_b_3

repl_args_b_4:
	mov	4(a0),a2
	mov	a2,K6_0(a3)
	add	$4,a3
repl_args_b_1:
	ret

push_arg_b:
	cmp	$2,d1
	jb	push_arg_b_1
	jne	push_arg_b_2
	cmp	d0,d1
	je	push_arg_b_1
push_arg_b_2:
	mov	8(a0),a0
	sub	$2,d1
push_arg_b_1:
	mov	(a0,d1,4),a0
	ret

del_args:
	mov	(a0),d1
	sub	d0,d1
	movswl	-2(d1),d0
	sub	$2,d0
	jge	del_args_2

	mov	d1,(a1)
	mov	4(a0),a2
	mov	a2,4(a1)	
	mov	8(a0),a2
	mov	a2,8(a1)
	ret

del_args_2:
	jne	del_args_3

	mov	d1,(a1)
	mov	4(a0),a2
	mov	a2,4(a1)
	mov	8(a0),a2
	mov	(a2),a2
	mov	a2,8(a1)
	ret

del_args_3:
	lea	-32(a4,d0,4),a2
	cmpl	end_heap,a2
	jae	del_args_gc
del_args_r_gc:
	mov	d1,(a1)
	mov	a4,8(a1)
	mov	4(a0),a2
	mov	8(a0),a0
	mov	a2,4(a1)

del_args_copy_args:
	mov	(a0),a2
	add	$4,a0
	mov	a2,(a4)
	add	$4,a4
	sub	$1,d0
	jg	del_args_copy_args

	ret

del_args_gc:
	call	collect_2l
	jmp	del_args_r_gc

#if 0
o__S_P2:
	mov	(a0),d0
	mov	8(a0),a0
	cmpw	$2,-2(d0)
	je	o__S_P2_2
	mov	(a0),a0
o__S_P2_2:
	ret

ea__S_P2:
	mov	4(a1),d0
	movl	$__indirection,(a1)
	mov	a0,4(a1)
	mov	d0,a1
	mov	(a1),d0
	testb	$2,d0
	jne	ea__S_P2_1
	
	mov	a0,K6_0(a3)
	add	$4,a3
	mov	a1,a0
	call	*d0
	mov	a0,a1
	mov	-4(a3),a0
	sub	$4,a3

ea__S_P2_1:
	mov	(a1),d0
	mov	8(a1),a1
	cmpw	$2,-2(d0)
	je	ea__S_P2_2
	mov	(a1),a1
ea__S_P2_2:
	mov	(a1),d0
	testb	$2,d0
	jne	ea__S_P2_3

	sub	$20,d0
	jmp	*d0

ea__S_P2_3:
	mov	d0,(a0)
	mov	4(a1),a2
	mov	a2,4(a0)
	mov	8(a1),a2
	mov	a2,8(a0)
	ret
#endif

#ifdef NOCLIB
tan_real:
	fptan
	fstsw	%ax
	testb	$0x04,%ah
	fstp	%st(0)
	jnz	tan_real_1
	ret

tan_real_1:
	fldl	NAN_real
	fstp	%st(1)
	ret

asin_real:
	fld	%st(0)
	fmul	%st(0)
	fsubrl	one_real
	fsqrt
	fpatan
	ret

acos_real:
	fld	%st(0)
	fmul	%st(0)
	fsubrl	one_real
	fsqrt
	fxch	%st(1)
	fpatan
	ret

atan_real:
	fldl	one_real
	fpatan
	ret

ln_real:
	fldln2
	fxch	%st(1)
	fyl2x
	ret

@c_log10:
	fldl	4(sp)
log10_real:
	fldlg2
	fxch	%st(1)
	fyl2x
	ret

exp_real:
	fldl2e
	subl	$16, sp
	fmulp	%st(1)

	fstcw	8(sp)
	movw	8(sp),%ax
	andw	$0xf3ff,%ax
	orw	$0x0400,%ax
	movw	%ax,10(sp)

exp2_real_:
	fld	%st
	fldcw	10(sp)
	frndint
	fldcw	8(sp)

	fsubr	%st,%st(1)
	fxch	%st(1)
	f2xm1
	faddl	one_real
	fscale
	addl	$16,sp
	fstp	%st(1)

	ret

pow_real:
	sub	$16,sp
	fstcw	8(sp)
	movw	8(sp),%ax
	andw	$0xf3ff,%ax

	fxch	%st(1)

	movw	%ax,10(sp)

	fcoml	zero_real
	fnstsw	%ax
	sahf
	jz	pow_zero
	jc	pow_negative

pow_real_:
	fyl2x
	jmp	exp2_real_

pow_negative:
	fld	%st(1)
	fldcw	10(sp)
	frndint
	fistl	12(sp)
	fldcw	8(sp)
	fsub	%st(2),%st

	fcompl	zero_real
	fstsw	%ax
	sahf
	jnz	pow_real_

	fchs
	fyl2x

	fld	%st
	fldcw	10(sp)
	frndint
	fldcw	8(sp)

	fsubr	%st,%st(1)
	fxch	%st(1)
	f2xm1
	faddl	one_real
	fscale

	testl	$1,12(sp)
	fstp	%st(1)
	jz	exponent_even
	fchs
exponent_even:
	add	$16,sp
	ret

pow_zero:
	fld	%st(1)
	fcompl	zero_real
	fnstsw	%ax
	sahf
	jbe	pow_real_

	fldl	zero_real
	fstp	%st(1)
	add	$16,sp
	ret

r_to_i_real:
	subl	$4,sp
	fistl	(sp)
	pop	d0
	ret

truncate_real:
	subl	$8,sp
	fstcw	(sp)
	movw	(sp),%ax
	orw	$0x0c00,%ax
	movw	%ax,2(sp)
	fldcw	2(sp)
	fistl	4(sp)
	fldcw	(sp)
	movl	4(sp),d0
	addl	$8,sp
	ret

entier_real:
	subl	$8,sp
	fstcw	(sp)
	movw	(sp),%ax
	andw	$0xf3ff,%ax
	orw	$0x0400,%ax
	movw	%ax,2(sp)
	fldcw	2(sp)
	fistl	4(sp)
	fldcw	(sp)
	movl	4(sp),d0
	addl	$8,sp
	ret

ceiling_real:
	subl	$8,sp
	fstcw	(sp)
	movw	(sp),%ax
	andw	$0xf3ff,%ax
	orw	$0x0800,%ax
	movw	%ax,2(sp)
	fldcw	2(sp)
	fistl	4(sp)
	fldcw	(sp)
	movl	4(sp),d0
	addl	$8,sp
	ret

round__real64:
	fistpll	12(%ecx)
	fldz
	ret

truncate__real64:
	subl	$4,sp
	fstcw	(sp)
	movw	(sp),%ax
	orw	$0x0c00,%ax
	movw	%ax,2(sp)
	fldcw	2(sp)
	fistpll	12(%ecx)
	fldcw	(sp)
	addl	$4,sp
	fldz
	ret

entier__real64:
	subl	$4,sp
	fstcw	(sp)
	movw	(sp),%ax
	andw	$0xf3ff,%ax
	orw	$0x0400,%ax
	movw	%ax,2(sp)
	fldcw	2(sp)
	fistpll	12(%ecx)
	fldcw	(sp)
	addl	$4,sp
	fldz
	ret

ceiling__real64:
	subl	$4,sp
	fstcw	(sp)
	movw	(sp),%ax
	andw	$0xf3ff,%ax
	orw	$0x0800,%ax
	movw	%ax,2(sp)
	fldcw	2(sp)
	fistpll	12(%ecx)
	fldcw	(sp)
	addl	$4,sp
	fldz
	ret

int64a__to__real:
	fildll	12(%ecx)
	fstp	%st(1)
	ret

@c_pow:
	fldl	4(sp)
	fldl	12(sp)
	call	pow_real
	fstp	%st(1)
	ret

@c_entier:
	fldl	4(sp)
	call	entier_real
	fstp	%st(0)
	ret
#else
	section	(tan_real)
tan_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@tan
	add	$8,sp
	ret

	section	(asin_real)	
asin_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@asin
	add	$8,sp
	ret

	section	(acos_real)
acos_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@acos
	add	$8,sp
	ret

	section	(atan_real)
atan_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@atan
	add	$8,sp
	ret

	section	(ln_real)
ln_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@log
	add	$8,sp
	ret

	section	(log10_real)
log10_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@log10
	add	$8,sp
	ret

	section	(exp_real)
exp_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@exp
	add	$8,sp
	ret

	section	(pow_real)
pow_real:
	sub	$16,sp
	fstpl	8(sp)
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@pow
	add	$16,sp
	ret

	section	(entier_real)
entier_real:
	sub	$8,sp
	fstpl	(sp)
	ffree	%st(0)
	ffree	%st(1)
	ffree	%st(2)
	ffree	%st(3)
	ffree	%st(4)
	ffree	%st(5)
	ffree	%st(6)
	ffree	%st(7)
	call	@floor
	add	$8,sp

r_to_i_real:
	fistl	int_to_real_scratch
	movl	int_to_real_scratch,d0
	ret
#endif

#ifdef NEW_DESCRIPTORS
# include "iap.s"
#endif
