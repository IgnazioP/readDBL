readDBL = function(n='0000', gridfile='grid.out', dblfile='dbl.out', x.out='x.out', y.out='y.out', z.out='z.out') 
   {
   # the function reads the data.????.dbl data cubes created by the MHD PLUTO code (Mignone et al. 2007)
   # PLUTO is a parallel code used for modeling of  astrophysical objects from star formation 
   # to supernovae explosions. 
   # Reference: Mignone et al.  
   # The Astrophysical Journal Supplement Series, Volume 170, Issue 1, pp. 228-242
   # https://ui.adsabs.harvard.edu/abs/2007ApJS..170..228M/abstract
   
   # read the x,y,z grid 
   grid.out = read.table(gridfile, fill=T)
   nf= count.fields(gridfile, comment.char='#')
   j = which(nf == 1)
   x = grid.out[seq(j[1]+1,j[2]-1,1),]  
   y = grid.out[seq(j[2]+1,j[3]-1,1),]  
   z = grid.out[seq(j[3]+1,length(nf),1),]
   
   # determine the mid point of coordinates in each direction
   xm = (x[,2] + x[,3])/2 
   ym = (y[,2] + y[,3])/2 
   zm = (z[,2] + z[,3])/2 
   
   # write separate files with x,y,z
   write.table(x, file=x.out, quote=F, row.names=F)
   write.table(y, file=y.out, quote=F, row.names=F)
   write.table(z, file=z.out, quote=F, row.names=F)
   
   # determine the length of the grid along x,y,z and the total number of cells (vsize) 
   xyzsize= grid.out[j,1]
   xsize= xyzsize[1]
   ysize= xyzsize[2]
   zsize= xyzsize[3]
   vsize= xsize*ysize*zsize
   
   # read the content of the dbl data cube and determine how many variables to read
   dbl.out = read.table(dblfile, as.is=T)[1,]
   vars = as.character(dbl.out[,-(1:6)])
   nvars= length(vars)
   starti = 1+ vsize * seq(0,nvars-1,1)
   endi   = vsize *  seq(1,nvars,1)
   
   # read the binary file data.????.dbl in the variable dbl, endianess is assumed "little" 
   ff = p('data.',n,'.dbl')
   dbl = readBin(ff, what='double', endian='little', size=8, n=vsize*length(vars))
   
   # create an empty list that will be populated with the variables with the "for" loop
   varlist = NULL
   for (i in (1:nvars)) {
     tmpvar =  dbl[starti[i]:endi[i]]
     varlist[[i]] = array(tmpvar,  dim=c(xsize, ysize, zsize))
      }
   varlist[[nvars+1]] = x
   varlist[[nvars+2]] = y
   varlist[[nvars+3]] = z
   varlist[[nvars+4]] = xm
   varlist[[nvars+5]] = ym
   varlist[[nvars+6]] = zm
   names(varlist) = c(vars, 'x', 'y', 'z', 'xm', 'ym', 'zm')
   # return the list of variables, these can be accessed with varlist$name, 
   # use str(varlist) to check its content.
   return(varlist)  
}

