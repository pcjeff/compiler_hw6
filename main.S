	.cpu cortex-a15
	.eabi_attribute 27, 3
	.eabi_attribute 28, 1
	.fpu vfpv3-d16
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"main.c"
	.section	.rodata
	.align	2
.LC0:
	.ascii	"%d\000"
	.text
	.align	2
	.global	_read_int
	.type	_read_int, %function
_read_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	str	fp, [sp, #-8]!
	str	lr, [sp, #4]
	add	fp, sp, #4
	sub	sp, sp, #8
	sub	r3, fp, #8
	movw	r0, #:lower16:.LC0
	movt	r0, #:upper16:.LC0
	mov	r1, r3
	bl	__isoc99_scanf
	ldr	r3, [fp, #-8]
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	ldr	fp, [sp]
	add	sp, sp, #4
	ldr	pc, [sp], #4
	.size	_read_int, .-_read_int
	.section	.rodata
	.align	2
.LC1:
	.ascii	"%f\000"
	.text
	.align	2
	.global	_read_float
	.type	_read_float, %function
_read_float:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	str	fp, [sp, #-8]!
	str	lr, [sp, #4]
	add	fp, sp, #4
	sub	sp, sp, #8
	sub	r3, fp, #8
	movw	r0, #:lower16:.LC1
	movt	r0, #:upper16:.LC1
	mov	r1, r3
	bl	__isoc99_scanf
	ldr	r3, [fp, #-8]	@ float
	fmsr	s15, r3
	fcpys	s0, s15
	sub	sp, fp, #4
	@ sp needed
	ldr	fp, [sp]
	add	sp, sp, #4
	ldr	pc, [sp], #4
	.size	_read_float, .-_read_float
	.align	2
	.global	_write_int
	.type	_write_int, %function
_write_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	str	fp, [sp, #-8]!
	str	lr, [sp, #4]
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	movw	r0, #:lower16:.LC0
	movt	r0, #:upper16:.LC0
	ldr	r1, [fp, #-8]
	bl	printf
	sub	sp, fp, #4
	@ sp needed
	ldr	fp, [sp]
	add	sp, sp, #4
	ldr	pc, [sp], #4
	.size	_write_int, .-_write_int
	.align	2
	.global	_write_float
	.type	_write_float, %function
_write_float:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	str	fp, [sp, #-8]!
	str	lr, [sp, #4]
	add	fp, sp, #4
	sub	sp, sp, #8
	fsts	s0, [fp, #-8]
	flds	s15, [fp, #-8]
	fcvtds	d7, s15
	movw	r0, #:lower16:.LC1
	movt	r0, #:upper16:.LC1
	fmrrd	r2, r3, d7
	bl	printf
	sub	sp, fp, #4
	@ sp needed
	ldr	fp, [sp]
	add	sp, sp, #4
	ldr	pc, [sp], #4
	.size	_write_float, .-_write_float
	.section	.rodata
	.align	2
.LC2:
	.ascii	"%s\000"
	.text
	.align	2
	.global	_write_str
	.type	_write_str, %function
_write_str:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	str	fp, [sp, #-8]!
	str	lr, [sp, #4]
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	movw	r0, #:lower16:.LC2
	movt	r0, #:upper16:.LC2
	ldr	r1, [fp, #-8]
	bl	printf
	mov	r0, r0	@ nop
	sub	sp, fp, #4
	@ sp needed
	ldr	fp, [sp]
	add	sp, sp, #4
	ldr	pc, [sp], #4
	.size	_write_str, .-_write_str
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	str	fp, [sp, #-8]!
	str	lr, [sp, #4]
	add	fp, sp, #4
	sub	sp, sp, #8
	bl	_start_MAIN
	str	r0, [fp, #-8]
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	ldr	fp, [sp]
	add	sp, sp, #4
	ldr	pc, [sp], #4
	.size	main, .-main
	.ident	"GCC: (Ubuntu/Linaro 4.8.2-16ubuntu4) 4.8.2"
	.section	.note.GNU-stack,"",%progbits

#include "output.s"
