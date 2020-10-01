!**************************************************************************************************
! Copyright (C), 2015, NuTheL Lab, Xi’an Jiaotong University, P.R. China
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
		integer :: i, j, k !循环变量，i行号，k列号
		integer :: N !矩阵行/列数
		real(kind = 8) :: L_U(N,N), A(N,N), b(N), x(N), L1(N,N), U1(N,N),y(N) !定义L_U(N,N)、A、分块下上三角阵L_(N,N)、 _U(N,N)，向量b和待求解向量x,中间向量y Ly=b
		real(kind = 8) :: sumlu = 0 !定义求和变量
		L_U = 1 !赋初值，主要是为了把LII=1
		L_U(1,:) = A(1,:) !填满U矩阵第一行
		L_U(:,1) = A(:,1)/L_U(1,1) !填满I矩阵第一列
		L_U(1,1) = A(1,1) !这里注意，由于是紧凑型，U1会被L1替代，故需要回调U1
		do i=2, N-1
			sumlu = 0 !归零
			do k=1, i-1
					sumlu = sumlu + L_U(i,k)*L_U(k,i) !求LiUi之积的和
			end do
			L_U(i,i) = A(i,i) - sumlu !求LU对角UII			
			do j=i+1, N		
				sumlu = 0 !归零
				do k=1, i-1
					sumlu = sumlu + L_U(i,k)*L_U(k,j) !求右侧UIJ
				end do
				L_U(i,j) = A(i,j) - sumlu				
				sumlu = 0 !归零
				do k=1, i-1
					sumlu = sumlu + L_U(j,k)*L_U(k,i) !求下侧LJI
				end do
				L_U(j,i) = (A(j,i) - sumlu)/L_U(i,i)				
			end do
		end do		
		sumlu = 0 !归零
		do k=1, N-1
			sumlu = sumlu + L_U(N,k)*L_U(k,N)
		end do
		L_U(N,N) = A(N,N) - sumlu
		
		L1(N,N) = 0 !矩阵分割
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
		y(1) = b(1)/L1(1,1) !先算第一行
		do k=2, N !下三角阵，正循环
			y(k) = b(k)
			do i=1, k-1
				y(k) = y(k) - L1(k,i)*y(i)
			end do
			y(k) = y(k)/L1(k,k) 
		end do !至此，Ux=y对应Aupx=bup
		call UPTRIsover(U1,y,x,N) !调用回带函数
	end subroutine LUsolve
END MODULE LU