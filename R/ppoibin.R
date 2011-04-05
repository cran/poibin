ppoibin <-
function(kk,pp,method="DFT-CF")
{
  switch(method,
  "DFT-CF"={
            nn=kk
            px=pp
            mm=length(nn)
            res=double(mm)
            n=length(px)
            avec=double(n+1)
            bvec=double(n+1)
            funcate=1
            ex=0
            tmp=.C("multi_bin_dft_cf",as.double(res),as.integer(nn),
            as.integer(mm),as.integer(n),as.double(px),as.double(avec),as.double(bvec),
            as.integer(funcate),as.double(ex),PACKAGE="poibin")
            res=tmp[[1]]
           },
  "RF"=    {
              nn=kk
              px=pp
              mm=length(nn)
              res=double(mm)
              n=length(px)
              mat=rep(0.0,(n+1)*(n+2))
              tmp=.C("multi_bin_bh",as.double(res),as.integer(nn),as.integer(mm),as.integer(n),as.double(px),as.double(mat),PACKAGE="poibin")
              res=tmp[[1]]
           },
  "RNA"=   {
               nn=kk
               px=pp
               muk=sum(px)
               sigmak=sqrt(sum(px*(1-px)))
               gammak=sum(px*(1-px)*(1-2*px))
               ind=gammak/(6*sigmak^3)
               kk=(nn+.5-muk)/sigmak
               vkk.r=pnorm(kk)+gammak/(6*sigmak^3)*(1-kk^2)*dnorm(kk)
               vkk.r[vkk.r<0]=0
               vkk.r[vkk.r>1]=1
               res=vkk.r
           },
  "NA"=    {
              nn=kk
              px=pp
              muk=sum(px)
              sigmak=sqrt(sum(px*(1-px)))
              gammak=sum(px*(1-px)*(1-2*px))
              kk=(nn+.5-muk)/sigmak
              res=pnorm(kk)
           }
  )

  return(res)
}
