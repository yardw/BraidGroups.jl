#https://zhuanlan.zhihu.com/p/647226253
Braid <- function(n=6, XLIM=c(-5, n+8),CEX=2, bgcol = "#00CCFF", smcol = "yellow", LWD=4)
{
    par(mai=rep(0,4), oma=rep(0,4), bg = bgcol)
    plot(0, 0, type = "n", xlim = XLIM, ylim = c(-2, 22),
         axes = TRUE, xlab = "", ylab = "") 
    redata <- function(n){
    x=CEX*(1:n); y=rep(20,n)
    if(n%%2==1){m<<-(n+1)/2; x=x-m}else{m<<-n/2;x=x-m+0.5}
    p <<- complex(0,x,y)   #点集合
    pp <<- c(p,p-20i)}
    redata(n)

    initial <- function(n){
    segments(Re(pp[1:n]), Im(pp[1:n]), Re(pp[(n+1):(2*n)]), Im(pp[(n+1):(2*n)]), lwd =LWD, col = smcol)
    points(pp, cex=CEX,col="red", pch=19)
    text(m+1.5, -1, "+", cex=3,col=smcol)
    text(m+1.5, 21, "-", cex=4,col=smcol)}
    initial(n)

    cross <- function(w, sign, k)    #w为交叉点的坐标；k为伸缩倍数
    {
        ld <- w-complex(0,1.4,k); ru <- w+complex(0,1.4,k)
        rect(xleft = Re(ld), ybottom = Im(ld), xright =Re(ru) , ytop =Im(ru) , border = NA, col = bgcol)
        x = seq(-1,1,0.001); y = k*tan(1.2*x)/tan(1.2)
        if(sign > 0)
        {
            z = -x + Re(w); y = y + Im(w)
            x = x + Re(w)
        }else{
            z = x + Re(w); y = y + Im(w)
            x = -x + Re(w)          
        }
        lines(y ~ z, col = smcol, lwd=LWD)
        lines(y[-(1:200)][-(1600:1801)] ~ x[-(1:200)][-(1600:1801)], col = bgcol, lwd=9)
        lines(y ~ x, col = smcol, lwd=LWD)  
    }

    group <- function(vec)
    {
       len = length(vec)
       k = 20/len
       w2 = seq(20-k/2,0,-k)
       w1 = Re(p[abs(vec)])+1
       w = complex(0,w1,w2)
       for(i in 1:len)cross(w[i], vec[i], k/2)
       points(pp, cex=CEX,col="red", pch=19)
    }
    vec = c()
    repeat
    {
            repeat
            {
                l <- locator(1)    
                ep <- complex(0,l$x,l$y)    #端点坐标
                if (any(Mod(pp-ep)<1.5))break    
            }
            w = which(Mod(pp-ep)<1.5)
            if(w<=n) w=-w else w=w%%n
            if(w%%n==0) {w = n;n=n+1}
            redata(n)
            vec = c(vec, w)
            rect(xleft = XLIM[1], ybottom = -2, xright =XLIM[2] , ytop =22 , border = NA, col = bgcol)
            initial(n); group(vec);print(list("operation"=vec))
    }
}
Braid()