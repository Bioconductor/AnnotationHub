#include <Rcpp.h>
#include "Node.h"

using namespace Rcpp;

class Trie {
public:
    // constructor, destructor
    Trie() {root = new Node();}
    ~Trie() 
    {
        Node* current = root;
        current->deleteAllNodes(); 
        delete root;
    }

    // build
    void buildTrie(SEXP x);
    void addWord(std::string s);
    // search
    Node* getPrefixNode(std::string s);
    // print
    SEXP printTrie();
    SEXP printTrie(SEXP x, int max);

private:
    Node* root;
};
