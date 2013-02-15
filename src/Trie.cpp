#include "Trie.h"
#include <stdexcept>

using namespace Rcpp;

// **************************************
// build
// **************************************
void Trie::buildTrie(SEXP x)
{
    string_vec args = Rcpp::as< string_vec >(x);
    Node* current = root;
    current->setParent(NULL);
    std::vector<std::string>::const_iterator itr;
    for(itr = args.begin(); itr != args.end(); itr++)
        addWord(*itr);
    compressTrie();
}

Node* Trie::findNode(std::string s)
{
    Node* current = root;
    for (unsigned int i = 0; i < s.length(); i++) {
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
            tmp->setValue(s.substr(i, 1));
            tmp->setParent(current);
            current->appendChild(tmp);
            current = tmp;
        }
        if (i == s.length() - 1) {
            current->setCumwords(1);
            current->setWord(true);
        }
    }
}

// **************************************
// compression
// **************************************
void Trie::compressTrie()
{
    Node* current = root;
    if (current != NULL)
        for (unsigned int i = 0; i < current->getChildren().size(); i++) {
            Node* tmp = current->getChildren().at(i);
            tmp->compressNode();
        }
}

// **************************************
// print
// **************************************
// print all words in trie
SEXP Trie::printTrie()
{
    string_vec res;
    string_vec *res_ptr;
    res_ptr = &res;
    Node* current = root;
    current->printNodeDFS("", res_ptr);
    return(Rcpp::wrap(res));
}

// print by prefix search 
SEXP Trie::printTrie(SEXP x, unsigned int max)
{
    BEGIN_RCPP
    if (max <= 0)
        throw std::range_error("max must be >= 0");
    std::string s = Rcpp::as< std::string >(x);
    string_vec res;
    string_vec *res_ptr;
    res_ptr = &res;

    Node* current = root;
    // check the first level of children before full search
    bool found = false;
    for (unsigned int i = 0; i < current->getChildren().size(); i++) {
        Node* tmp = current->getChildren().at(i);
        if ((tmp->getValue().find(s) != std::string::npos) ||
            (s.find(tmp->getValue()) != std::string::npos))
            found = true;
    }
    // full search
    if (found)
        current->findPrefixNode(s)->printNodeBFS(s, res_ptr, max);
    return(Rcpp::wrap(res));
    END_RCPP
}

RCPP_MODULE(TrieModule){
    using namespace Rcpp;

    class_<Trie>("Trie")
    .constructor()
    .method("buildTrie", &Trie::buildTrie,
        "Construct a Trie from a vector of strings")
    .method("printTrie", (SEXP (Trie::*)(SEXP, unsigned int))(&Trie::printTrie))
    .method("printTrie", (SEXP (Trie::*)())(&Trie::printTrie))
    ;
}
