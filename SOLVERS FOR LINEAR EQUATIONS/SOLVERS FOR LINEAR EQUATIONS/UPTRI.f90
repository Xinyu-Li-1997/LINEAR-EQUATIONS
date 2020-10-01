!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi��an Jiaotong University, P.R. China
! File name: UPTRI.f90
! Author: Li Xinyu, Zhou Xingguang        
! Version: 1.0        
! Date: 2020/9/29
! Description: To calculate x in up triangle
! Others: None
! Function List: None
! Sunroutine List: Gausssolve
! Module list:  None
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!**************************************************************************************************
MODULE UPTRI
	IMPLICIT NONE
	CONTAINS
	subroutine UPTRIsover(A,b,x,N)
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/29
	! Coded by:	Li Xinyu 
	! Calls: None      
	! Called By: Gausssolve(A,b,x,N),LUsolve(A,b,x,N)    
	! Input: A,b,N      
	! Output: x
	! Return: x     
	! Others:       
	!*************************************************
		implicit none
		integer i,j,N !i�кţ�j�кţ�N�����С�����
		real(kind = 8) :: A(N,N), b(N), x(N) !�������A������b�ʹ��������x
		!���濪ʼ�ش�
		x(N) = b(N)/A(N,N) !�������һ��
		do i=N-1, 1, -1 !i��������ѭ��
			x(i) = b(i)
			do j=i+1, N !һ���еļ���
				x(i) = x(i) - x(j)*A(i,j)
			end do
			x(i) = x(i)/A(i,i) !�����Ե�ǰ��x(i)��ϵ��
		end do
	end subroutine UPTRIsover
END MODULE UPTRI