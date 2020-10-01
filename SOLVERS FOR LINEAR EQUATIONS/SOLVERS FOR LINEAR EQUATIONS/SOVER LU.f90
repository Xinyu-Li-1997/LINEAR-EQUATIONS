!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi��an Jiaotong University, P.R. China
! File name: SOVER LU.f90
! Author: Li Xinyu    
! Version: 1.0        
! Date: 2020/9/30
! Description: This is the sover of LU
! Others: None
! Function List: None
! Sunroutine List: LUsolve
! Module list:  UPTRI
! History:     
!    1. Date: 2020/9/30
!        Author: Li Xinyu
!        Modification: Began developing the program
!**************************************************************************************************
MODULE LU
	USE UPTRI
	IMPLICIT NONE
	CONTAINS
	subroutine LUsolve(A,b,x,N) 
	!*************************************************
	! Version:1.0     
	! Date: 2020/9/30
	! Coded by:	Li Xinyu 
	! Calls: downtri (A,L,U,N)
	! Called By: SOLVERSFORLINEAREQUATIONS
	! Input: A,b,N      
	! Output:
	! Return:x     
	! Others:       
	!*************************************************
		implicit none
		integer :: i, j, k !ѭ��������i�кţ�k�к�
		integer :: N !������/����
		real(kind = 8) :: L_U(N,N), A(N,N), b(N), x(N), L1(N,N), U1(N,N),y(N) !����L_U(N,N)��A���ֿ�����������L_(N,N)�� _U(N,N)������b�ʹ��������x,�м�����y Ly=b
		real(kind = 8) :: sumlu = 0 !������ͱ���
		L_U = 1 !����ֵ����Ҫ��Ϊ�˰�LII=1
		L_U(1,:) = A(1,:) !����U�����һ��
		L_U(:,1) = A(:,1)/L_U(1,1) !����I�����һ��
		L_U(1,1) = A(1,1) !����ע�⣬�����ǽ����ͣ�U1�ᱻL1���������Ҫ�ص�U1
		do i=2, N-1
			sumlu = 0 !����
			do k=1, i-1
					sumlu = sumlu + L_U(i,k)*L_U(k,i) !��LiUi֮���ĺ�
			end do
			L_U(i,i) = A(i,i) - sumlu !��LU�Խ�UII			
			do j=i+1, N		
				sumlu = 0 !����
				do k=1, i-1
					sumlu = sumlu + L_U(i,k)*L_U(k,j) !���Ҳ�UIJ
				end do
				L_U(i,j) = A(i,j) - sumlu				
				sumlu = 0 !����
				do k=1, i-1
					sumlu = sumlu + L_U(j,k)*L_U(k,i) !���²�LJI
				end do
				L_U(j,i) = (A(j,i) - sumlu)/L_U(i,i)				
			end do
		end do		
		sumlu = 0 !����
		do k=1, N-1
			sumlu = sumlu + L_U(N,k)*L_U(k,N)
		end do
		L_U(N,N) = A(N,N) - sumlu
		
		L1(N,N) = 0 !����ָ�
		do i=1, N
			L1(i,i) = 1
			U1(i,i) = L_U(i,i)
			do j=i+1, N
				L1(j,i) = L_U(j,i)
				U1(i,j) = L_U(i,j)
			end do
		end do
		!LUx=b
		!Ly=b
		!Ux=y
		y(1) = b(1)/L1(1,1) !�����һ��
		do k=2, N !����������ѭ��
			y(k) = b(k)
			do i=1, k-1
				y(k) = y(k) - L1(k,i)*y(i)
			end do
			y(k) = y(k)/L1(k,k) 
		end do !���ˣ�Ux=y��ӦAupx=bup
		call UPTRIsover(U1,y,x,N) !���ûش�����
	end subroutine LUsolve
END MODULE LU