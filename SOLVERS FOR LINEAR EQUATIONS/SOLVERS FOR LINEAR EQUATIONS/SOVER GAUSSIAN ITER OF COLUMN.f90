!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi��an Jiaotong University, P.R. China
! File name: SOVER GAUSSIAN ITER OF COLUMN.f90
! Author: Li Xinyu    
! Version: 1.0        
! Date: 2020/9/29
! Description: This is the sover of GAUSSIAN ITER OF COLUMN
! Others: None
! Function List: None
! Sunroutine List: Gausssolve
! Module list:  UPTRI
! History:     
!    1. Date: 2020/9/29
!        Author: Li Xinyu
!        Modification: Began developing the program
!**************************************************************************************************
MODULE GAUSSIAN_COLUMN
	USE UPTRI
	IMPLICIT NONE
	
	!INTEGER,PARAMETER:: DBL = 8
	CONTAINS
	subroutine Gausssolve(A,b,x,N) 
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/29
	! Coded by:	Li Xinyu 
	! Calls: UPTRIsover(Aup,bup,x,N)      
	! Called By: readtxt(A,b,x,N)
	! Input: A,b,N      
	! Output:
	! Return:x     
	! Others:       
	!*************************************************
		implicit none
		integer :: i, k !ѭ��������i�кţ�k�к�
		integer :: N !������/����
		integer :: idmax !��Ԫ�ر��
		real(kind = 8) :: elmax !��Ԫ�ؾ���ֵ 
		real(kind = 8) :: temp !�ݴ�ͬ�����е�ϵ���� 
		real(kind = 8) :: A(N,N), b(N), x(N) !�������A������b�ʹ��������x
		real(kind = 8) :: Aup(N,N), bup(N) 
		real(kind = 8) :: Ab(N,N+1) !�������
		real(kind = 8) :: vtemp1(N+1), vtemp2(N+1) !�������е��м�����
		
		Ab(1:N,1:N) = A !������Ab��ϵ������ֵΪA
		Ab(:,N+1) = b !������Ab��β������ֵΪb
		!===============================================================
		!����Ԫ��ȥ������
		do k=1, N-1 !k��Ϊ�б�ţ���ѭ��ɨ����
			elmax = dabs(Ab(k,k)) !��¼��ֵ
			idmax = k !��¼��λ��
			do i=k+1, N !��ɨ���У�iΪ�к�
				if (dabs(Ab(i,k)) > elmax) then
					elmax = dabs(Ab(i,k)) !��ȡ�����ֵ
					idmax = i
				end if
			end do !��������������Ԫ�صĲ��ң����潻����˳��
			vtemp1 = Ab(k,:)
			vtemp2 = Ab(idmax ,:)
			Ab(k,:) = vtemp2
			Ab(idmax ,:) = vtemp1
			!��ʼ��Ԫ
			do i = k+1, N
				temp = Ab(i,k)/Ab(k,k)
				Ab(i,:) = Ab(i,:) - temp*Ab(k,:) !k+1�и���k����Ԫ
			end do
		end do 
		!���ˣ�Ab��ת��Ϊ��������
		!===============================================================
		Aup(:,:) = Ab(1:N,1:N) !���������Ϊϵ���󣬴���
		bup(:) = Ab(:,N+1) !���������Ϊβ���󣬴���
		!write (*,*) "��ϵ������"
		!write (*,*) Aup
		!write (*,*) "��β������"
		!write (*,*) bup
		call UPTRIsover(Aup,bup,x,N) !���ûش�����
	end subroutine Gausssolve
END MODULE GAUSSIAN_COLUMN
	