library(readr)
sample1 <- read_csv("sample1.csv")
View(sample1)


options(max.print=1000000)


# WELO

#basemodel - mean
basemodelW = lm(WElo~1,data = sample1)
summary(basemodelW)

# linear Model
modelW = lm(formula = WElo~ECO+PlyCount+Result+WResult+BResult+WConversions+BConversions+WFavPiece+BFavPiece+WCaptureMovesCount+BCaptureMovesCount+WOrdMovesCount+BOrdMovesCount+WPiecesCount+BPiecesCount+Rr+Rn+Rb+Rq+Rk+Rp+rR+rN+rB+rQ+rK+rP+Nr+Nn+Nb+Nq+Nk+Np+nR+nN+nB+nQ+nK+nP+Br+Bn+Bb+Bq+Bk+Bp+bR+bN+bB+bQ+bK+bP+Qr+Qn+Qb+Qq+Qk+Qp+qR+qN+qB+qQ+qK+qP+Kr+Kn+Kb+Kq+Kk+Kp+kR+kN+kB+kQ+kK+kP+Pr+Pn+Pb+Pq+Pk+Pp+pR+pN+pB+pQ+pK+pP+BCheck+WCheck+BChecked+WChecked+WScore+BScore, data = sample1)
summary(modelW)
#MSE
mean(modelW$residuals^2)

#Comparing base model(mean) with model 
anova(basemodelW, modelW)

#parsimonious model
parsModelW = step(modelW)
summary(parsModelW)
plot(parsModelB)


# BElo

#BaseModel
basemodelB = lm(BElo~1, data=sample1)
summary(basemodelB)

#LinearModel
modelB = lm(formula = BElo~ECO+PlyCount+Result+WResult+BResult+WConversions+BConversions+WFavPiece+BFavPiece+WCaptureMovesCount+BCaptureMovesCount+WOrdMovesCount+BOrdMovesCount+WPiecesCount+BPiecesCount+Rr+Rn+Rb+Rq+Rk+Rp+rR+rN+rB+rQ+rK+rP+Nr+Nn+Nb+Nq+Nk+Np+nR+nN+nB+nQ+nK+nP+Br+Bn+Bb+Bq+Bk+Bp+bR+bN+bB+bQ+bK+bP+Qr+Qn+Qb+Qq+Qk+Qp+qR+qN+qB+qQ+qK+qP+Kr+Kn+Kb+Kq+Kk+Kp+kR+kN+kB+kQ+kK+kP+Pr+Pn+Pb+Pq+Pk+Pp+pR+pN+pB+pQ+pK+pP+BCheck+WCheck+BChecked+WChecked+WScore+BScore, data = sample1)
summary(modelB)
#MSE
mean(modelB$residuals^2)

#Comparing base model(mean) with model 
anova(basemodelB, modelB)

#parsimonious model
parsModelB = step(modelB)

summary(parsModelB)
plot(parsModelB)
