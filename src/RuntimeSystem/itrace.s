
#undef DEBUG_PROFILER

#define MODULE_NAMES

#define d0 %eax
#define d1 %ebx
#define a0 %ecx
#define a1 %edx
#define a2 %ebp
#define a3 %esi
#define a4 %edi
#define sp %esp

#if defined(_WINDOWS_)
# define	align(n) .align (1<<n)
#else
# define	align(n) .align n
#endif

	.global	init_profiler
	.global	profile_r
	.global profile_l
	.global profile_l2
	.global profile_n
	.global profile_n2
	.global profile_s
	.global profile_s2
	.global profile_t
	.global	write_profile_information
	.global	write_profile_stack

#ifdef LINUX
	.global	@malloc
#else
	.global	@allocate_memory
#endif
	.global	__STRING__
	.global	writeFC
	.global	writeFI
	.global	print_error
	.global	@ab_stack_size
	.global	@ew_print_string
	.global	@ew_print_char
	.global	@stack_trace_depth

#define	next		0
#define	name		4
#define	FunctionProfile	8

	.text
profile_t:
	subl	$4,profile_stack_pointer
	ret

profile_r:
	subl	$4,profile_stack_pointer
	ret

profile_l:
	push	d1
	mov	(a2),d1

	test	d1,d1
	je	allocate_function_profile_record_l
allocate_function_profile_record_lr:
	mov	profile_stack_pointer,a2

#ifdef DEBUG_PROFILER
	testl	(d1),d1
#endif
	mov	d1,(a2)
	add	$4,a2
	
	mov	a2,profile_stack_pointer

	pop	d1
	ret

allocate_function_profile_record_l:
	call	allocate_function_profile_record
	jmp	allocate_function_profile_record_lr

profile_l2:
	push	d1
	mov	(a2),d1

	test	d1,d1
	je	allocate_function_profile_record_l2
allocate_function_profile_record_l2r:	
	mov	profile_stack_pointer,a2
	
#ifdef DEBUG_PROFILER
	testl	(d1),d1
#endif
	mov	d1,(a2)
	mov	d1,4(a2)
	add	$8,a2

	mov	a2,profile_stack_pointer

	pop	d1
	ret

allocate_function_profile_record_l2:
	call	allocate_function_profile_record
	jmp	allocate_function_profile_record_l2r

profile_n:
	push	d1
	mov	(a2),d1
	
	test	d1,d1
	je	allocate_function_profile_record_n
allocate_function_profile_record_nr:
	mov	profile_stack_pointer,a2

#ifdef DEBUG_PROFILER
	testl	(d1),d1
#endif
	mov	d1,(a2)
	add	$4,a2

	mov	a2,profile_stack_pointer

	pop	d1
	ret

allocate_function_profile_record_n:
	call	allocate_function_profile_record
	jmp	allocate_function_profile_record_nr

profile_n2:
	push	d1
	mov	(a2),d1

	test	d1,d1
	je	allocate_function_profile_record_n2
allocate_function_profile_record_n2r:	
	mov	profile_stack_pointer,a2

#ifdef DEBUG_PROFILER
	testl	(d1),d1
#endif
	mov	d1,(a2)
	mov	d1,4(a2)
	add	$8,a2

	mov	a2,profile_stack_pointer

	pop	d1
	ret

allocate_function_profile_record_n2:
	call	allocate_function_profile_record
	jmp	allocate_function_profile_record_n2r

profile_s2:
	push	d1
	mov	(a2),d1
	
	test	d1,d1
	je	allocate_function_profile_record_s2
allocate_function_profile_record_s2r:
	mov	profile_stack_pointer,a2
	
#ifdef DEBUG_PROFILER
	testl	(d1),d1
#endif
	movl	d1,(a2)
	movl	d1,4(a2)
	add	$8,a2
	mov	a2,profile_stack_pointer

	pop	d1
	ret

allocate_function_profile_record_s2:
	call	allocate_function_profile_record
	jmp	allocate_function_profile_record_s2r

profile_s:
	push	d1
	movl	(a2),d1
	
	test	d1,d1
	je	allocate_function_profile_record_s
allocate_function_profile_record_sr:
	mov	profile_stack_pointer,a2
	
#ifdef DEBUG_PROFILER
	testl	(d1),d1
#endif
	movl	d1,(a2)
	add	$4,a2

	mov	a2,profile_stack_pointer

	pop	d1
	ret

allocate_function_profile_record_s:
	call	allocate_function_profile_record
	jmp	allocate_function_profile_record_sr


/ argument: a2: function name adress-4
/ result:   d1: function profile record adress

allocate_function_profile_record:
	push	d0
	mov	global_n_free_records_in_block,d0
	mov	global_last_allocated_block,d1

	test	d0,d0
	jne	no_alloc

	push	d1
	push	a0
	push	a1

	pushl	$512*FunctionProfile
#ifdef LINUX
	call	@malloc
#else
	call	@allocate_memory
#endif
	add	$4,sp

	test	d0,d0

	pop	a1
	pop	a0
	pop	d1

	je	no_memory

	mov	d0,d1
	mov	$512,d0
	mov	d1,global_last_allocated_block

no_alloc:	
	dec	d0
	mov	d0,global_n_free_records_in_block
	lea	FunctionProfile(d1),d0
	mov	d0,global_last_allocated_block
	
	mov	global_profile_records,d0
	mov	a2,name(d1)

	mov	d0,next(d1)
	mov	d1,global_profile_records
	
	mov	d1,(a2)
	pop	d0
	ret

no_memory:
	movl	$not_enough_memory_for_profiler,a2
	pop	d0
	jmp	print_error

write_profile_information:
	ret
	
write_profile_stack:
	mov	profile_stack_pointer,d0

	test	d0,d0
	je	stack_not_initialised

	push	d0
	
	push	$stack_trace_string
	call	@ew_print_string
	add	$4,sp
	
	pop	d0
	
/	mov	$12,a2
	movl	@stack_trace_depth,a2
write_functions_on_stack:
	mov	-4(d0),d1
	sub	$4,d0

	test	d1,d1
	je	end_profile_stack

	push	d0
	mov	name(d1),a0

	push	a2

#ifdef MODULE_NAMES
	movl	-4(a0),a1
#endif

	add	$4,a0

#ifdef MODULE_NAMES
	pushl	(a1)
	addl	$4,a1
	pushl	a1
#endif

	push	a0
	call	@ew_print_string
	add	$4,sp

#ifdef MODULE_NAMES
	pushl	$module_string
	call	@ew_print_string
	add	$4,sp

	call	@ew_print_text
	addl	$8,sp

	pushl	$']'
	call	@ew_print_char
	add	$4,sp
#endif

	pushl	$10
	call	@ew_print_char
	add	$4,sp

	pop	a2
	pop	d0

	sub	$1,a2
	jne	write_functions_on_stack
	
end_profile_stack:
stack_not_initialised:
	ret

init_profiler:
	pushl	@ab_stack_size
#ifdef LINUX
	call	@malloc
#else
	call	@allocate_memory
#endif
	add	$4,sp
	
	test	d0,d0
	je	init_profiler_error

	push	d0
	
	mov	$start_string,a2
	call	allocate_function_profile_record

	pop	a1

	mov	d1,4(a1)
	movl	$0,(a1)
	add	$8,a1
	mov	a1,profile_stack_pointer

 	mov	end_heap,a1
	sub	a4,a1
	add	$32,a1	
	mov	a1,global_n_bytes_free
	ret

init_profiler_error:
	movl	$0,profile_stack_pointer
	movl	$not_enough_memory_for_profile_stack,a2
	jmp	print_error

	.data
	align (2)

global_n_free_records_in_block:	.long 0
/ 0 n free records in block
global_last_allocated_block:	.long 0
/ 4 latest allocated block
global_profile_records:		.long 0
/ 8 profile record list
profile_stack_pointer:		.long 0
global_n_bytes_free:		.long 0	

	align	(2)
@stack_trace_depth:
	.long	12
#ifdef MODULE_NAMES
# if 0
/ m_system also defined in cgistartup.s
m_system:
	.long	6
	.ascii	"System"
	.byte	0
	.byte	0
# endif
	.long	m_system
#endif
start_string:
	.long	0
	.asciz	"start"
	align	(2)
not_enough_memory_for_profile_stack:
	.ascii	"not enough memory for profile stack"
	.byte	10
	.byte	0
not_enough_memory_for_profiler:
	.ascii	"not enough memory for profiler"
	.byte	10
	.byte	0
stack_trace_string:
	.ascii	"Stack trace:"
	.byte	10
	.byte	0
#ifdef MODULE_NAMES
module_string:
	.asciz	" [module: "
#endif
	align	(2)
