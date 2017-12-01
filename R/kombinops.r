#' Count \emph{k}-permutations of \emph{n} elements (without repetition)
#'
#' Count possible arrangements of a fixed length \code{k} of elements
#' taken from a given set of size \code{n}.
#' In other words, \code{k}-permutations of \code{n} (without repetition of elements from the initial set -
#' any element of the initial set can occure in every arrangement one or zero times).
#' The result is equal to zero when \code{k}>\code{n} and \eqn{ \frac{n!}{(n-k)!} }{ n! / (n-k)! } otherwise.
#'
#' @details For 2-permutations of 3 without repetition we have 6 variants. If,
#' for example, the initial set is (1,2,3), then permutations will be:
#' (1,2), (1,3), (2,1), (2,3), (3,1) and (3,2).
#'
#' @param n The number of elements in the initial set.
#' @param k The number of elements in every resulting arrangement.
#'
#' @return The number of \code{k}-permutations of \code{n}
#'     (without repetition).
#'
#' @examples
#' \dontrun{
#' Ank(2,3)
#' Ank(3,2)
#' Ank(length(c("l","u","c","k")),3)
#' }
#'
#' @export
Ank <-function(n,k)
{
    if (k>n)
        return (0)
    return(factorial(n)/factorial(n-k))
}

#' Count \emph{k}-permutations of \emph{n} elements (with repetition)
#'
#' Count possible arrangements of a fixed length \code{k} of elements
#' taken from a given set of size \code{n}. In other words, \code{k}-permutations of \code{n}
#' (with possible repetition of elements from the initial set in every arrangment any number of times).
#' The result is equal to \eqn{ n^k }{ n^k }.
#'
#' @details For 2-permutations of 3 with repetition we have 9 variants. If,
#' for example, the initial set is (1,2,3), then permitations will be:
#' (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (1,1), (2,2) and (3,3).
#'
#' @inheritParams Ank
#'
#' @return The number of \code{k}-permutations of \code{n} (with possible repetition of
#'     elements from the initial set any number of times)
#'
#' @examples
#' \dontrun{
#' Ank_(2,3)
#' Ank_(3,2)
#' Ank_(length(c("l","u","c","k")),3)
#' }
#'
#' @export
Ank_ <-function(n,k)
    return(n^k)

#' Count \emph{k}-combinations of \emph{n} elements (without repetition)
#'
#' Count possible combinations of a fixed length \code{k} of elements
#' taken from a given set of size \code{n}. A combination is a selection of items
#' from a collection, such that (unlike permutations) the order of selection does not matter.
#' The result is equal to zero if \code{k}>\code{n} and \eqn{ \frac{n!}{k!*(n-k)!} }{ n! / k!(n-k)! }
#' otherwise.
#'
#' @details For 2-combinations of 3 without repetition we have 3 variants. If,
#' for example, the initial set is (1,2,3), then combinations will be:
#' (1,2), (1,3) and (2,3).
#'
#' @inheritParams Ank
#'
#' @return The number of \code{k}-combinations of \code{n} (without repetition of elements
#'     from the initial set - any element of the initial set can occure in any
#'     combination one or zero times).
#'
#' @examples
#' \dontrun{
#' Cnk(2,3)
#' Cnk(3,2)
#' Cnk(length(c("1","2","3","4")),3)
#' }
#'
#' @export
Cnk <-function(n,k)
    return(Ank(n,k)/factorial(k))

#' Count \emph{k}-combinations of \emph{n} elements (with repetition)
#'
#' Count possible combinations of a fixed length \code{k} of elements
#' taken from a given set of size \code{n}. A combination is a selection of items
#' from a collection, such that (unlike permutations) the order of selection does not matter.
#' The result is equal to \eqn{ \frac{(n+k-1)!}{k!(n-1)!} }{ (n+k-1)!/(k!*(n-1)!) }.
#'
#' @details For 2-combinations of 3 with repetition we have 6 variants. If,
#' for example, the initial set is (1,2,3), then combinations will be:
#' (1,2), (1,3), (2,3), (1,1), (2,2) and (3,3).
#'
#' @inheritParams Ank
#'
#' @return The number of \code{k}-combinations of \code{n} (with repetition of elements
#'     from the initial set - any element of the initial set can occure in any
#'     combination any number of times).
#'
#' @examples
#' \dontrun{
#' Cnk_(2,3)
#' Cnk_(3,2)
#' Cnk_(length(c("1","2","3","4")),3)
#' }
#'
#' @export
Cnk_ <-function(n,k)
    return(Cnk(n+k-1,k))

#' Count permutations of multiset
#'
#' If \emph{M} is a finite multiset, then a multiset permutation is an ordered
#' arrangement of elements of \emph{M} in which each element appears exactly as
#' often as is its multiplicity in \emph{M}. An anagram of a word having some repeated
#' letters is an example of a multiset permutation. If the multiplicities of the elements of \emph{M}
#' (taken in some order) are \eqn{ m_1, m_2, ..., m_l }{ m1, m2, ..., ml } and
#' their sum (i.e., the size of \emph{M}) is \emph{n}, then the number of multiset permutations
#' of \emph{M} is given by the multinomial coefficient \eqn{ \frac{n!}{m_1!m_2!...m_l!} }{ n!/(m1!m2!...ml!) }.
#'
#'
#' @details For example, the number of distinct anagrams of the word MISSISSIPPI is
#' \eqn{ \frac{11!}{1!4!4!2!}=34650 }{ 11!/(1!4!4!2!)=34650 }.
#'
#' @param nel An integer vector of counts of each element in multiset.
#'
#' @return The number of permutations of multiset.
#'
#' @examples
#' \dontrun{
#' # Permutations of the word "FERRARY"
#' letters<-strsplit("FERRARY", NULL)[[1]]
#' tw<-table(letters)
#' P(tw)
#' }
#'
#' @export
P <-function(nel)
    return(factorial(sum(nel))/prod(factorial(nel)))

gnm <-function(n,m)
    return(Cnk(n+m,n))
