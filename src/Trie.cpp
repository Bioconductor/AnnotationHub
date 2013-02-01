#include "Trie.h"

using namespace std;

SEXP Trie::printTrie()
{
    string_vec res;
    string_vec *res_ptr;
    res_ptr = &res;
    Node* current = root;
    current->printNodeDFS("", res_ptr);
    return(Rcpp::wrap(res));
}

SEXP Trie::printTrie(SEXP x, int max)
{
    BEGIN_RCPP
    if (max < 0)
        throw std::range_error("'max' must be >= 0");

    std::string s = Rcpp::as< std::string >(x);
    string_vec res;
    string_vec *res_ptr;
    res_ptr = &res;
    Node* current = root;
    current = getPrefixNode(s);
    if (current != 0)
        current->printNodeBFS(s, res_ptr, max);
    return(Rcpp::wrap(res));
    END_RCPP
}

Node* Trie::getPrefixNode(std::string s)
{
    Node* current = root;
    for (unsigned int i = 0; i < s.length(); i++)
    {
        Node* tmp = current->findChild(s[i]);
        if (tmp == NULL)
            return tmp;
        current = tmp;
    }
    return current;
}

void Trie::addWord(std::string s)
{
    Node* current = root;
    if (s.length() == 0) {
        return;
    }
    for (unsigned int i = 0; i < s.length(); i++) {
        Node* child = current->findChild(s[i]);
        if (child != NULL) {
            current = child;
        } else {
            Node* tmp = new Node();
            tmp->setValue(s[i]);
            tmp->setParent(current);
            current->appendChild(tmp);
            current = tmp;
        }
        if (i == s.length() - 1)
            current->setWordMarker();
    }
}

void Trie::buildTrie(SEXP x)
{
    string_vec args = Rcpp::as< string_vec >(x);
    Node* current = root;
    current->setParent(NULL);
    std::vector<std::string>::const_iterator itr;
    for(itr = args.begin(); itr != args.end(); itr++)
        addWord(*itr);
}

RCPP_MODULE(TrieModule){
    using namespace Rcpp;

    class_<Trie>("Trie")
    .constructor()
    .method("buildTrie", &Trie::buildTrie,
        "Construct a Trie from a vector of strings")
    .method("printTrie", (SEXP (Trie::*)(SEXP, int))(&Trie::printTrie))
    .method("printTrie", (SEXP (Trie::*)())(&Trie::printTrie))
    ;
}
