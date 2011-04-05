qpoibin <-
function(qq,pp)
{
 nn=1
 px=pp
 mm=length(qq)
 res=double(mm)
 n=length(px)
 avec=double(n+1)
 bvec=double(n+1)
 funcate=3
 ex=qq
 tmp=.C("multi_bin_dft_cf",as.double(res),as.integer(nn),
 as.integer(mm),as.integer(n),as.double(px),as.double(avec),as.double(bvec),
 as.integer(funcate),as.double(ex),PACKAGE="poibin")
 res=tmp[[1]]
 return(res)
}