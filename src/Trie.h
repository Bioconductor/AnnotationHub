#include <Rcpp.h>
#include "Node.h"

using namespace Rcpp;

class Trie {
public:
    // constructor, destructor
    Trie() { root = new Node(); }
    ~Trie() 
    {
        Node* current = root;
        current->deleteAllNodes(); 
        delete root;
    }

    // build
    void buildTrie(SEXP x);
    void addWord(std::string word);
    Node* findNode(std::string s);

    // compress
    void compressTrie();

    // print
    SEXP printTrie();
    SEXP printTrie(SEXP x, unsigned int max);

private:
    Node* root;
};
