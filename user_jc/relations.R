# Sections and contractions for non-functional (not left total right unique) relations?! 

# Conditions for preserving functionality

# functional, bi-(in/sur)jective relation encoded as element of 2^(X^2)
X = c("a","b","c","d") #row/colnames implicit
Y = c("d", "e", "f", "g")

R = rbind(c(0,1,0,0), c(1,0,0,0), c(0,0,0,1), c(0,0,1,0))
I = diag(c(1,1,1,1))
# relation g has bijective property if exists hg=i_a and gk=i_b, which implies h=k="g^-1".
t(R) %*% R == I
R %*% t(R)  == I

R = rbind(c(0,1,0,0), c(0,1,0,0), c(0,0,0,1), c(0,0,1,0))
R 
t(R) # the non-functional inverse relation 
t(R) %*% R   # converse relation is not functional
R %*% t(R)  

R = rbind(c(0,1,0,0, 0, 0), c(0,1,0,0, 0, 0), c(0,0,0,1, 0, 0), c(0,0,1,0, 0, 0), c(0,0,1,0, 0, 0), c(0,0,1,0, 0, 0))
R 
t(R) # inverse relation non-functional
t(R) %*% R  #  the number of times I hit each element of the Codomain
R %*% t(R)  # inverse image equivalence relation. equivalence classes of the preimage?

############################################################
# Identity matrix viewed as identity relation on finite sets.
############################################################

# Matrix boolean (not linear) algebra for non-sparse (characteristic function) encoding of relation R.

R = matrix(rep(0, 21*100), 21)
x = 1:21
fx = c(-10:10) ^2
for(.x in x){ 
  R[.x, fx[.x]] = 1
}
image(R)

# Fails:
map(x, function(.x) R[.x, fx[.x]] = 1)

R %*% t(R) # equivalence relation induced on dom(R) by R
t(R) %*% R # size of preimage (equiv classes) for each element of cod(R), i.e. the number of times each element of codomain is hit
image(t(R) %*% R)
image(R %*% t(R) )

############################################################
# package relations
############################################################

library(relations)
predicates = c("binary", "ternary", "quaternary", "left_total", "right_total",
               "surjective", "functional", "injective", "bijective", "endorelation", "homogeneous", "crisp",
               "complete", "match", "strongly_complete", "reflexive", "irreflexive", "coreflexive", "symmetric", "asymmetric", 
               "antisymmetric", "transitive", 
               "negatively_transitive", "quasitransitive", "Ferrers", "semitransitive", "trichotomous", 
               "Euclidean", "equivalence", "weak_order", "preference", "preorder", "quasiorder", 
               "partial_order", "linear_order", "strict_partial_order", "strict_linear_order", "tournament", "interval_order", 
               "semiorder", "acyclic", "cyclic")

map(predicates, relation_is(R1, .x))


############################################################
# a second order predicate: a predicate on relations
# second order properties, each defined in terms of first order (elements) of a set
############################################################
properties_of_my_relation = function(R){
  map(set_names(predicates), ~ relation_is(R, .x)) %>%
    enframe() %>% 
    unnest(value) %>% 
    filter(value == TRUE) %>% 
    print(n = Inf)
}

R = as.relation(sigs$CVD_mRNA)
properties_of_my_relation(R) # slow if big relation
R = sigs %>% unlist() %>% as.relation()


sigs %>% unlist() %>% as.relation() %>% relation_is_equivalence()
sigs %>% unlist() %>% as.relation() %>% relation_incidence() %>% image



