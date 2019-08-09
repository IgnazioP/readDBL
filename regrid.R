regrid = function(x,y,img,nx=NA,ny=NA){

if(is.na(nx)) nx = length(x)
if(is.na(ny)) ny = length(y)

tmpimg = array(0,dim=c(nx,length(y)))

xr = seq(min(x), max(x), len=nx)
yr = seq(min(y), max(y), len=ny)

for (j in 1:length(y) ) {
    tmpimg[,j] = approx(x,img[,j],xout=xr)$y
    }

img2 = tmpimg
tmpimg = array(0,dim=c(nx,ny))

for (i in 1:length(xr) ) {
    tmpimg[i,] = approx(y,img2[i,],xout=yr)$y
    }

return(list(x=xr, y=yr, z=tmpimg))

}
