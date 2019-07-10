      program FiltroInvONeill
      real dx,ab(50),s,x(100),sum,a(20),T(50),p(3),e(2)
      integer n,k,npd,npf,j,h,npfm1,nc,ncm1
C     defino los coeficientes del filtro
C     las abscisas donde se quiere el valor de la resistividad aparente
      data a/0.003042,-0.001198,0.01284,0.02350,0.08688,0.2374
     #,0.6194,1.1817,0.4248,-3.4507,2.7044,-1.1324,0.3930,-0.1436,0.0581
     #2,-0.02521,0.01125,-0.004978,0.002072,-0.000318/

C     corrimiento,n§ puntos por d‚cada, puntos que necesita el filtro
C     espaciamiento
      s=-0.13069
      npd=6
      npf=20
      dx=log(10.)/npd
      write(*,*) "_____________________________________________________"
      write(*,*) "   El programa calcula las Resistividades Aparentes "
      write(*,*) "    usando el filtro de OïNeill y el algoritmo de   "
      write(*,*) "         Sunde, para el c lculo de la FTR.          "
      write(*,*) "_____________________________________________________"
      write(*,*)
      write(*,*) "-----------------------------------------------------"
      write(*,*) " Como entrada se necesita que en la misma ubicaci¢n"
      write(*,*) " de este ejecutable, exista un archivo Abs.dat que "
      write(*,*) "   tenga en forma de columna, los valores de las "
      write(*,*) "   abscisas donde se desea conocer el valor de CRA"
      write(*,*) "-----------------------------------------------------"
      open(10,file="Abs.dat")
      n=0
33    n=n+1
         read(10,*,end=44) ab(n)
      go to 33
44    n=n-1
      close(10)
      write(*,*) "             "
      write(*,*) "+++++++++++++++++++++++++++++++++++++++++++++++++++++"
      write(*,*) " A continuaci¢n requiere escribir los par metros del "
      write(*,*) " corte geol‚ctrico:              "
      write(*,*) "   - n£mero de capas (max=10)"
      write(*,*) "   - Resistividades de una"
      write(*,*) "   - Espesores de uno"
      write(*,*) "+++++++++++++++++++++++++++++++++++++++++++++++++++++"

      write(*,*) "             "
      write(*,*) "Ingresar n£mero de capas del corte"
      read(*,*) nc
      do i=1,nc
         write(*,*) i,"ø Resistividad"
         read(*,*) p(i)
      enddo
      ncm1=nc-1
      write(*,*) "             "
      do i=1,nc-1
         write(*,*) i,"ø Espesor"
         read(*,*) e(i)
      enddo
      open(12,file="RA.dat")
C     siendo n abscisas efect£o la siguiente iteraci¢n
      do k=1,n
C     calculo la ubicaci¢n de puntos centrado en la abscisa
        h=0
        do j=1,npf
            h=h+1
            x(h)=exp(log(ab(k))+s+(j-15)*dx)
        enddo

C     calculo la transformada de resistividad aparente en el vector x
        call TRS(x,T,h,nc,p,e)
C     calculo la convoluci¢n entre el filtro y la transformada de resist
C     que me da la curva de resistividad aparente
        sum=0.
        npfm1=npf+1

        do h=1,npf
          sum=a(npfm1-h)*T(h)+sum
        enddo
        write(12,*) ab(k),sum
      enddo
      write(*,*)
      write(*,*)"Listo                     "
      write(*,*)"Generado el archivo RA.dat"
      pause
      close(12)
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        Subroutine TRS(x,T,h,nc,p,e)
        real p(10),e(9),x(100),L,M,aux1,aux2,aux3,T(30)
        integer i,nc,ncm1,h,j
        data L,M,aux1,aux2,aux3/5*0./
        ncm1=nc-1
C     calculo las transformada de resistividad con el algoritmo de Sunde
        do j=1,h
           L=(p(nc)-p(nc-1))/(p(nc)+p(nc-1))
           aux1=L*exp(-2/x(j)*e(nc-1))
           M=(1.+aux1)/(1-aux1)
           if(ncm1.lt.2) go to 55
           do i=2,ncm1
              aux2=p(nc-i+1)*M
              L=(aux2-p(nc-i))/(aux2+p(nc-i))
              aux3=L*exp(-2./x(j)*e(nc-i))
              M=(1.+aux3)/(1.-aux3)
           enddo
55         T(j)=p(1)*M
        enddo
        end
