
rmark_using_reversal:
	push	rsi
	push	rsi
	mov	rsi,1
	jmp	rmarkr_node

rmark_using_reversal_:
	sub	rcx,8
	push	rbx
	push	rsi
	cmp	rcx,rbx
	ja	rmark_no_undo_reverse_1
	mov	qword ptr [rsi],rcx
	mov	qword ptr [rcx],rax
rmark_no_undo_reverse_1:
	mov	rsi,1
	jmp	rmarkr_arguments

rmark_array_using_reversal:
	push	rbx
	push	rsi
	cmp	rcx,rbx
	ja	rmark_no_undo_reverse_2
	mov	qword ptr [rsi],rcx
	lea	rax,__ARRAY__+2[rip]
	mov	qword ptr [rcx],rax
rmark_no_undo_reverse_2:
	mov	rsi,1
	att_jmp	rmarkr_arguments

rmarkr_hnf_2:
	or	qword ptr [rcx],2
	mov	rbp,qword ptr 8[rcx]
	mov	qword ptr 8[rcx],rsi
	lea	rsi,8[rcx]
	mov	rcx,rbp

rmarkr_node:
	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rcx

	cmp	rax,qword ptr heap_size_64_65_offset[r9]
	jnc	rmarkr_next_node_after_static

	mov	rbx,rax
	and	rax,31*8
	shr	rbx,8

	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	mov	ebp,dword ptr [rdi+rbx*4]

	test	rbp,rax
	jne	rmarkr_next_node

	or	rbp,rax
	mov	dword ptr [rdi+rbx*4],ebp

rmarkr_arguments:
	mov	rax,qword ptr [rcx]
	test	al,2
	je	rmarkr_lazy_node

	movzx	rbp,word ptr (-2)[rax]
	test	rbp,rbp
	je	rmarkr_hnf_0

	add	rcx,8

	cmp	rbp,256
	jae	rmarkr_record

	sub	rbp,2
	att_je	rmarkr_hnf_2
	jc	rmarkr_hnf_1

rmarkr_hnf_3:
	mov	rdx,qword ptr 8[rcx]

	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rdx

	mov	rbx,rax
	and	rax,31*8
	shr	rbx,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	test	eax,[rdi+rbx*4]
	jne	rmarkr_shared_argument_part

	or	dword ptr [rdi+rbx*4],eax

rmarkr_no_shared_argument_part:
	or	qword ptr [rcx],2
	mov	qword ptr 8[rcx],rsi
	add	rcx,8

	or	qword ptr [rdx],1
	lea	rdx,[rdx+rbp*8]

	mov	rbp,qword ptr [rdx]
	mov	qword ptr [rdx],rcx
	mov	rsi,rdx
	mov	rcx,rbp
	att_jmp	rmarkr_node

rmarkr_shared_argument_part:
	cmp	rdx,rcx
	att_ja	rmarkr_hnf_1

	mov	rbx,qword ptr [rdx]
	lea	rax,(8+2+1)[rcx]
	mov	qword ptr [rdx],rax
	mov	qword ptr 8[rcx],rbx
	att_jmp	rmarkr_hnf_1

rmarkr_record:
	sub	rbp,258
	je	rmarkr_record_2
	jb	rmarkr_record_1

rmarkr_record_3:
	movzx	rbp,word ptr (-2+2)[rax]
	sub	rbp,1
	jb	rmarkr_record_3_bb
	je	rmarkr_record_3_ab
	dec	rbp
	je	rmarkr_record_3_aab
	att_jmp	rmarkr_hnf_3

rmarkr_record_3_bb:
	mov	rdx,qword ptr (16-8)[rcx]
	sub	rcx,8

	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rdx

	mov	rbp,rax
	and	rax,31*8
	shr	rbp,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	or	dword ptr [rdi+rbp*4],eax

	cmp	rdx,rcx
	att_ja	rmarkr_next_node

	add	eax,eax
	jne	rmarkr_bit_in_same_word1
	inc	rbp
	mov	rax,1
rmarkr_bit_in_same_word1:
	test	eax,dword ptr [rdi+rbp*4]
	je	rmarkr_not_yet_linked_bb

	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rcx

	add	rax,2*8

	mov	rbp,rax 
	and	rax,31*8
	shr	rbp,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	or	dword ptr [rdi+rbp*4],eax

	mov	rbp,qword ptr [rdx]
	lea	rax,(16+2+1)[rcx]
	mov	qword ptr 16[rcx],rbp 
	mov	qword ptr [rdx],rax 
	att_jmp	rmarkr_next_node

rmarkr_not_yet_linked_bb:
	or	dword ptr [rdi+rbp*4],eax
	mov	rbp,qword ptr [rdx]
	lea	rax,(16+2+1)[rcx]
	mov	qword ptr 16[rcx],rbp 
	mov	qword ptr [rdx],rax 
	att_jmp	rmarkr_next_node

rmarkr_record_3_ab:
	mov	rdx,qword ptr 8[rcx]

	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rdx

	mov	rbp,rax
	and	rax,31*8
	shr	rbp,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	or	dword ptr [rdi+rbp*4],eax

	cmp	rdx,rcx
	att_ja	rmarkr_hnf_1

	add	eax,eax 
	jne	rmarkr_bit_in_same_word2
	inc	rbp
	mov	rax,1
rmarkr_bit_in_same_word2:
	test	eax,dword ptr [rdi+rbp*4]
	je	rmarkr_not_yet_linked_ab

	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rcx

	add	rax,8

	mov	rbp,rax
	and	rax,31*8
	shr	rbp,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	or	dword ptr [rdi+rbp*4],eax

	mov	rbp,qword ptr [rdx]
	lea	rax,(8+2+1)[rcx]
	mov	qword ptr 8[rcx],rbp
	mov	qword ptr [rdx],rax
	att_jmp	rmarkr_hnf_1

rmarkr_not_yet_linked_ab:
	or	dword ptr [rdi+rbp*4],eax
	mov	rbp,qword ptr [rdx]
	lea	rax,(8+2+1)[rcx]
	mov	qword ptr 8[rcx],rbp 
	mov	qword ptr [rdx],rax
	att_jmp	rmarkr_hnf_1

rmarkr_record_3_aab:
	mov	rdx,qword ptr 8[rcx]

	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rdx 

	mov	rbp,rax
	and	rax,31*8
	shr	rbp,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	test	eax,dword ptr [rdi+rbp*4]
	att_jne	rmarkr_shared_argument_part
	or	dword ptr [rdi+rbp*4],eax

	add	qword ptr [rcx],2
	mov	qword ptr 8[rcx],rsi
	add	rcx,8

	mov	rsi,qword ptr [rdx]
	mov	qword ptr [rdx],rcx 
	mov	rcx,rsi
	lea	rsi,1[rdx]
	att_jmp	rmarkr_node

rmarkr_record_2:
	cmp	word ptr (-2+2)[rax],1
	att_ja	rmarkr_hnf_2
	att_je	rmarkr_hnf_1
	sub	rcx,8
	att_jmp	rmarkr_next_node

rmarkr_record_1:
	cmp	word ptr (-2+2)[rax],0
	att_jne	rmarkr_hnf_1
	sub	rcx,8
	att_jmp	rmarkr_next_node

rmarkr_lazy_node_1:
	jne	rmarkr_selector_node_1

rmarkr_hnf_1:
	mov	rbp,qword ptr [rcx]
	mov	qword ptr [rcx],rsi 

	lea	rsi,2[rcx]
	mov	rcx,rbp 
	att_jmp	rmarkr_node

rmarkr_indirection_node:
	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	lea	rbx,(-8)[rcx+rbx]

	mov	rax,rbx
	and	rax,31*8
	shr	rbx,8
	lea	r11,bit_clear_table2[rip]
	mov	eax,dword ptr [r11+rax]
	and	dword ptr [rdi+rbx*4],eax

	mov	rcx,qword ptr [rcx]
	att_jmp	rmarkr_node

rmarkr_selector_node_1:
	add	rbp,3
	att_je	rmarkr_indirection_node

	mov	rdx,qword ptr [rcx]

	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	add	rbx,rdx
	shr	rbx,3

	add	rbp,1
	jle	rmarkr_record_selector_node_1

	push	rax
	mov	rax,rbx

	shr	rbx,5
	and	rax,31

	lea	r11,bit_set_table[rip]
	mov	eax,dword ptr [r11+rax*4]
	mov	ebx,dword ptr [rdi+rbx*4]
	and	rbx,rax 

	pop	rax
	att_jne	rmarkr_hnf_1

	mov	rbx,qword ptr [rdx]
	test	bl,2
	att_je	rmarkr_hnf_1

	cmp	word ptr (-2)[rbx],2
	jbe	rmarkr_small_tuple_or_record

rmarkr_large_tuple_or_record:
	mov	rbx,qword ptr 16[rdx]
	add	rbx,qword ptr neg_heap_p3_offset[r9]
	shr	rbx,3

	push	rax
	mov	rax,rbx

	shr	rbx,5
	and	rax,31

	lea	r11,bit_set_table[rip]
	mov	eax,dword ptr [r11+rax*4]
	mov	ebx,dword ptr [rdi+rbx*4]
	and	rbx,rax

	pop	rax
	att_jne	rmarkr_hnf_1

	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	lea	rbx,(-8)[rcx+rbx]

	push	rcx

	movsxd	rcx,dword ptr (-8)[rax]
	add	rax,rcx

	mov	rcx,rbx
	and	rcx,31*8
	shr	rbx,8
	lea	r11,bit_clear_table2[rip]
	mov	ecx,dword ptr [r11+rcx]
	and	dword ptr [rdi+rbx*4],ecx

	movzx	eax,word ptr (4-8)[rax]
	cmp	rax,16
	jl	rmarkr_tuple_or_record_selector_node_2
	mov	rdx,qword ptr 16[rdx]
	je	rmarkr_tuple_selector_node_2
	mov	rcx,qword ptr (-24)[rdx+rax]
	pop	rdx
	lea	rax,__indirection[rip]
	mov	qword ptr (-8)[rdx],rax
	mov	qword ptr [rdx],rcx
	att_jmp	rmarkr_node

rmarkr_tuple_selector_node_2:
	mov	rcx,qword ptr [rdx]
	pop	rdx
	lea	rax,__indirection[rip]
	mov	qword ptr (-8)[rdx],rax
	mov	qword ptr [rdx],rcx
	att_jmp	rmarkr_node

rmarkr_record_selector_node_1:
	je	rmarkr_strict_record_selector_node_1

	push	rax
	mov	rax,rbx

	shr	rbx,5
	and	rax,31

	lea	r11,bit_set_table[rip]
	mov	eax,dword ptr [r11+rax*4]
	mov	ebx,dword ptr [rdi+rbx*4]
	and	rbx,rax 

	pop	rax
	att_jne	rmarkr_hnf_1

	mov	rbx,qword ptr [rdx]
	test	bl,2
	att_je	rmarkr_hnf_1

	cmp	word ptr (-2)[rbx],258
	att_jbe	rmarkr_small_tuple_or_record

	mov	rbx,qword ptr 16[rdx]
	add	rbx,qword ptr neg_heap_p3_offset[r9]
	shr	rbx,3

	push	rax
	mov	rax,rbx
	shr	rbx,5
	and	rax,31
	lea	r11,bit_set_table[rip]
	mov	eax,dword ptr [r11+rax*4]
	mov	ebx,dword ptr [rdi+rbx*4]
	and	rbx,rax
	pop	rax
	att_jne	rmarkr_hnf_1

rmarkr_small_tuple_or_record:
	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	lea	rbx,(-8)[rcx+rbx]

	push	rcx

	movsxd	rcx,dword ptr(-8)[rax]
	add	rax,rcx

	mov	rcx,rbx
	and	rcx,31*8
	shr	rbx,8
	lea	r11,bit_clear_table2[rip]
	mov	ecx,dword ptr [r11+rcx]
	and	dword ptr [rdi+rbx*4],ecx 

	movzx	eax,word ptr (4-8)[rax]
	cmp	rax,16
	att_jle	rmarkr_tuple_or_record_selector_node_2
	mov	rdx,qword ptr 16[rdx]
	sub	rax,24
rmarkr_tuple_or_record_selector_node_2:
	mov	rcx,qword ptr [rdx+rax]
	pop	rdx
	lea	rax,__indirection[rip]
	mov	qword ptr (-8)[rdx],rax
	mov	qword ptr [rdx],rcx
	att_jmp	rmarkr_node

rmarkr_strict_record_selector_node_1:
	push	rax
	mov	rax,rbx

	shr	rbx,5
	and	rax,31

	lea	r11,bit_set_table[rip]
	mov	eax,dword ptr [r11+rax*4]
	mov	ebx,dword ptr [rdi+rbx*4]
	and	rbx,rax 

	pop	rax 
	att_jne	rmarkr_hnf_1

	mov	rbx,qword ptr [rdx]
	test	bl,2
	att_je	rmarkr_hnf_1

	cmp	word ptr (-2)[rbx],258
	jbe	rmarkr_select_from_small_record

	mov	rbx,qword ptr 16[rdx]
	add	rbx,qword ptr neg_heap_p3_offset[r9]

	push	rax
	mov	rax,rbx

	shr	rbx,8
	and	rax,31*8

	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	mov	ebx,dword ptr [rdi+rbx*4]
	and	rbx,rax

	pop	rax
	att_jne	rmarkr_hnf_1

rmarkr_select_from_small_record:
	movsxd	rbx,dword ptr(-8)[rax]
	add	rax,rbx
	sub	rcx,8

	movzx	ebx,word ptr (4-8)[rax]
	cmp	rbx,16
	jle	rmarkr_strict_record_selector_node_2
	add	rbx,qword ptr 16[rdx]
	mov	rbx,qword ptr (-24)[rbx]
	jmp	rmarkr_strict_record_selector_node_3
rmarkr_strict_record_selector_node_2:
	mov	rbx,qword ptr [rdx+rbx]
rmarkr_strict_record_selector_node_3:
	mov	qword ptr 8[rcx],rbx

	movzx	ebx,word ptr (6-8)[rax]
	test	rbx,rbx
	je	rmarkr_strict_record_selector_node_5
	cmp	rbx,16
	jle	rmarkr_strict_record_selector_node_4
	mov	rdx,qword ptr 16[rdx]
	sub	rbx,24
rmarkr_strict_record_selector_node_4:
	mov	rbx,qword ptr [rdx+rbx]
	mov	qword ptr 16[rcx],rbx
rmarkr_strict_record_selector_node_5:

	mov	rax,qword ptr ((-8)-8)[rbx]
	mov	qword ptr [rcx],rax
	att_jmp	rmarkr_next_node

/* a2,d1: free */

rmarkr_next_node:
	test	rsi,3
	jne	rmarkr_parent

	mov	rbp,qword ptr (-8)[rsi]
	mov	rbx,3
	
	and	rbx,rbp
	sub	rsi,8

	cmp	rbx,3
	je	rmarkr_argument_part_cycle1

	mov	rdx,qword ptr 8[rsi]
	mov	qword ptr [rsi],rdx

rmarkr_c_argument_part_cycle1:
	cmp	rcx,rsi 
	ja	rmarkr_no_reverse_1

	mov	rdx,qword ptr [rcx]
	lea	rax,(8+1)[rsi]
	mov	qword ptr 8[rsi],rdx
	mov	qword ptr [rcx],rax

	or	rsi,rbx
	mov	rcx,rbp
	xor	rcx,rbx
	att_jmp	rmarkr_node

rmarkr_no_reverse_1:
	mov	qword ptr 8[rsi],rcx
	mov	rcx,rbp
	or	rsi,rbx
	xor	rcx,rbx
	att_jmp	rmarkr_node

rmarkr_lazy_node:
	movsxd	rbp,dword ptr (-4)[rax]
	test	rbp,rbp
	att_je	rmarkr_next_node

	add	rcx,8

	sub	rbp,1
	att_jle	rmarkr_lazy_node_1

	cmp	rbp,255
	jge	rmarkr_closure_with_unboxed_arguments

rmarkr_closure_with_unboxed_arguments_:
	or	qword ptr [rcx],2
	lea	rcx,[rcx+rbp*8]

	mov	rbp,qword ptr [rcx]
	mov	qword ptr [rcx],rsi
	mov	rsi,rcx
	mov	rcx,rbp
	att_jmp	rmarkr_node

rmarkr_closure_with_unboxed_arguments:
/* (a_size+b_size)+(b_size<<8) */
/*	add	rbp,1 */
	mov	rax,rbp
	and	rbp,255
	shr	rax,8
	sub	rbp,rax
/*	sub	rbp,1 */
	att_jg	rmarkr_closure_with_unboxed_arguments_
	att_je	rmarkr_hnf_1
	sub	rcx,8
	att_jmp	rmarkr_next_node

rmarkr_hnf_0:
	lea	rbp,dINT+2[rip]
	cmp	rax,rbp
	je	rmarkr_int_3

	lea	rbp,CHAR+2[rip]
	cmp	rax,rbp
 	je	rmarkr_char_3

	jb	rmarkr_no_normal_hnf_0

	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	add	rbx,rcx

	mov	rcx,rbx
	and	rcx,31*8
	shr	rbx,8
	lea	r11,bit_clear_table2[rip]
	mov	ecx,dword ptr [r11+rcx]
	and	dword ptr [rdi+rbx*4],ecx 

	lea	rcx,((-8)-2)[rax]
	att_jmp	rmarkr_next_node_after_static

rmarkr_int_3:
	mov	rbp,qword ptr 8[rcx]
	cmp	rbp,33
	att_jnc	rmarkr_next_node

	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	add	rbx,rcx

	mov	rcx,rbx
	and	rcx,31*8
	shr	rbx,8
	lea	r11,bit_clear_table2[rip]
	mov	ecx,dword ptr [r11+rcx]
	shl	rbp,4
	and	dword ptr [rdi+rbx*4],ecx 

	lea	rcx,small_integers[rip]
	add	rcx,rbp
	att_jmp	rmarkr_next_node_after_static

rmarkr_char_3:
	mov	rbx,qword ptr neg_heap_p3_offset[r9]

	movzx	rax,byte ptr 8[rcx]
	add	rbx,rcx

	mov	rbp,rbx
	and	rbp,31*8
	shr	rbx,8
	lea	r11,bit_clear_table2[rip]
	mov	ebp,dword ptr [r11+rbp]
	and	dword ptr [rdi+rbx*4],ebp

	shl	rax,4
	lea	rcx,static_characters[rip]
	add	rcx,rax
	att_jmp	rmarkr_next_node_after_static

rmarkr_no_normal_hnf_0:
	lea	rbp,__ARRAY__+2[rip]
	cmp	rax,rbp
	att_jne	rmarkr_next_node

	mov	rax,qword ptr 16[rcx]
	test	rax,rax
	je	rmarkr_lazy_array

	movzx	rbx,word ptr (-2+2)[rax]
	test	rbx,rbx
	je	rmarkr_b_array

	movzx	rax,word ptr (-2)[rax]
	test	rax,rax
	att_je	rmarkr_b_array

	sub	rax,256
	cmp	rbx,rax
	je	rmarkr_a_record_array

rmarkr_ab_record_array:
	mov	rdx,qword ptr 8[rcx]
	add	rcx,16
	push	rcx

	imul	rdx,rax 
	shl	rdx,3

	sub	rax,rbx
	add	rcx,8
	add	rdx,rcx
	att_call	reorder
	
	pop	rcx
	mov	rax,rbx
	imul	rax,qword ptr (-8)[rcx]
	jmp	rmarkr_lr_array

rmarkr_b_array:
	mov	rax,qword ptr neg_heap_p3_offset[r9]
	add	rax,rcx
	add	rax,8
	mov	rbp,rax
	and	rax,31*8
	shr	rbp,8
	lea	r11,bit_set_table2[rip]
	mov	eax,dword ptr [r11+rax]
	or	dword ptr [rdi+rbp*4],eax
	att_jmp	rmarkr_next_node

rmarkr_a_record_array:
	mov	rax,qword ptr 8[rcx]
	add	rcx,16
	cmp	rbx,2
	att_jb	rmarkr_lr_array

	imul	rax,rbx
	att_jmp	rmarkr_lr_array

rmarkr_lazy_array:
	mov	rax,qword ptr 8[rcx]
	add	rcx,16

rmarkr_lr_array:
	mov	rbx,qword ptr neg_heap_p3_offset[r9]
	add	rbx,rcx 
	shr	rbx,3
	add	rbx,rax 

	mov	rdx,rbx
	and	rbx,31
	shr	rdx,5
	lea	r11,bit_set_table[rip]
	mov	ebx,dword ptr [r11+rbx*4]
	or	dword ptr [rdi+rdx*4],ebx

	cmp	rax,1
	jbe	rmarkr_array_length_0_1

	mov	rdx,rcx 
	lea	rcx,[rcx+rax*8]

	mov	rax,qword ptr [rcx]
	mov	rbx,qword ptr [rdx]
	mov	qword ptr [rdx],rax
	mov	qword ptr [rcx],rbx 
	
	mov	rax,qword ptr (-8)[rcx]
	sub	rcx,8
	add	rax,2
	mov	rbx,qword ptr (-8)[rdx]
	sub	rdx,8
	mov	qword ptr [rcx],rbx
	mov	qword ptr [rdx],rax

	mov	rax,qword ptr (-8)[rcx]
	sub	rcx,8
	mov	qword ptr [rcx],rsi
	mov	rsi,rcx
	mov	rcx,rax
	att_jmp	rmarkr_node

rmarkr_array_length_0_1:
	lea	rcx,-16[rcx]
	att_jb	rmarkr_next_node

	mov	rbx,qword ptr 24[rcx]
	mov	rbp,qword ptr 16[rcx]
	mov	qword ptr 24[rcx],rbp
	mov	rbp,qword ptr 8[rcx]
	mov	qword ptr 16[rcx],rbp
	mov	qword ptr 8[rcx],rbx
	add	rcx,8
	att_jmp	rmarkr_hnf_1

/* a2: free */

rmarkr_parent:
	mov	rbx,rsi
	and	rbx,3

	and	rsi,-4
	je	end_rmarkr

	sub	rbx,1
	je	rmarkr_argument_part_parent

	mov	rbp,qword ptr [rsi]

	cmp	rcx,rsi
	ja	rmarkr_no_reverse_2

	mov	rdx,rcx
	lea	rax,1[rsi]
	mov	rcx,qword ptr [rdx]
	mov	qword ptr [rdx],rax

rmarkr_no_reverse_2:
	mov	qword ptr [rsi],rcx 
	lea	rcx,(-8)[rsi]
	mov	rsi,rbp
	att_jmp	rmarkr_next_node

rmarkr_argument_part_parent:
	mov	rbp,qword ptr [rsi]

	mov	rdx,rsi
	mov	rsi,rcx
	mov	rcx,rdx

rmarkr_skip_upward_pointers:
	mov	rax,rbp
	and	rax,3
	cmp	rax,3
	jne	rmarkr_no_upward_pointer

	lea	rdx,(-3)[rbp]
	mov	rbp,qword ptr (-3)[rbp]
	att_jmp	rmarkr_skip_upward_pointers

rmarkr_no_upward_pointer:
	cmp	rsi,rcx
	ja	rmarkr_no_reverse_3

	mov	rbx,rsi
	mov	rsi,qword ptr [rsi]
	lea	rax,1[rcx]
	mov	qword ptr [rbx],rax

rmarkr_no_reverse_3:
	mov	qword ptr [rdx],rsi
	lea	rsi,(-8)[rbp]

	and	rsi,-4

	mov	rdx,rsi
	mov	rbx,3

	mov	rbp,qword ptr [rsi]

	and	rbx,rbp
	mov	rax,qword ptr 8[rdx]

	or	rsi,rbx
	mov	qword ptr [rdx],rax

	cmp	rcx,rdx
	ja	rmarkr_no_reverse_4

	mov	rax,qword ptr [rcx]
	mov	qword ptr 8[rdx],rax
	lea	rax,(8+2+1)[rdx]
	mov	qword ptr [rcx],rax
	mov	rcx,rbp
	and	rcx,-4
	att_jmp	rmarkr_node

rmarkr_no_reverse_4:
	mov	qword ptr 8[rdx],rcx
	mov	rcx,rbp
	and	rcx,-4
	att_jmp	rmarkr_node

rmarkr_argument_part_cycle1:
	mov	rax,qword ptr 8[rsi]
	push	rdx

rmarkr_skip_pointer_list1:
	mov	rdx,rbp
	and	rdx,-4
	mov	rbp,qword ptr [rdx]
	mov	rbx,3
	and	rbx,rbp
	cmp	rbx,3
	att_je	rmarkr_skip_pointer_list1

	mov	qword ptr [rdx],rax
	pop	rdx
	att_jmp	rmarkr_c_argument_part_cycle1

rmarkr_next_node_after_static:
	test	rsi,3
	jne	rmarkr_parent_after_static

	mov	rbp,qword ptr (-8)[rsi]
	mov	rbx,3
	
	and	rbx,rbp
	sub	rsi,8

	cmp	rbx,3
	je	rmarkr_argument_part_cycle2
	
	mov	rax,qword ptr 8[rsi]
	mov	qword ptr [rsi],rax

rmarkr_c_argument_part_cycle2:
	mov	qword ptr 8[rsi],rcx
	mov	rcx,rbp
	or	rsi,rbx
	xor	rcx,rbx
	att_jmp	rmarkr_node

rmarkr_parent_after_static:
	mov	rbx,rsi
	and	rbx,3

	and	rsi,-4
	je	end_rmarkr_after_static

	sub	rbx,1
	je	rmarkr_argument_part_parent_after_static

	mov	rbp,qword ptr [rsi]
	mov	qword ptr [rsi],rcx
	lea	rcx,(-8)[rsi]
	mov	rsi,rbp
	att_jmp	rmarkr_next_node
	
rmarkr_argument_part_parent_after_static:
	mov	rbp,qword ptr [rsi]

	mov	rdx,rsi
	mov	rsi,rcx
	mov	rcx,rdx

/*	movl	rbp,qword ptr [rdx] */
rmarkr_skip_upward_pointers_2:
	mov	rax,rbp
	and	rax,3
	cmp	rax,3
	att_jne	rmarkr_no_reverse_3

	lea	rdx,(-3)[rbp]
	mov	rbp,qword ptr (-3)[rbp]
	att_jmp	rmarkr_skip_upward_pointers_2

rmarkr_argument_part_cycle2:
	mov	rax,qword ptr 8[rsi]
	push	rdx

rmarkr_skip_pointer_list2:
	mov	rdx,rbp 
	and	rdx,-4
	mov	rbp,qword ptr [rdx]
	mov	rbx,3
	and	rbx,rbp
	cmp	rbx,3
	att_je	rmarkr_skip_pointer_list2

	mov	qword ptr [rdx],rax
	pop	rdx
	att_jmp	rmarkr_c_argument_part_cycle2

end_rmarkr_after_static:
	mov	rsi,qword ptr [rsp]
	add	rsp,16
	mov	qword ptr [rsi],rcx
	jmp	rmarkr_next_stack_node

end_rmarkr:
	pop	rsi
	pop	rbx

	cmp	rcx,rbx
	ja	rmarkr_no_reverse_5

	mov	rdx,rcx
	lea	rax,1[rsi]
	mov	rcx,qword ptr [rcx]
	mov	qword ptr [rdx],rax

rmarkr_no_reverse_5:
	mov	qword ptr [rsi],rcx

rmarkr_next_stack_node:
	cmp	rsp,qword ptr end_stack_offset[r9]
	jae	rmarkr_end

	mov	rcx,qword ptr [rsp]
	mov	rsi,qword ptr 8[rsp]
	add	rsp,16

	cmp	rcx,1
	att_ja	rmark_using_reversal

	test	qword ptr _flags[rip],4096
	att_je	rmark_next_node_
	att_jmp	rmarkp_next_node_

rmarkr_end:
	test	qword ptr _flags[rip],4096
	att_je	rmark_next_node
	att_jmp	rmarkp_next_node
