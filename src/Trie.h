#include <Rcpp.h>
#include "Node.h"


using namespace Rcpp;

class Trie {
public:
    // constructor, deconstructor 
    Trie() { root = new Node(); }
    void deleteTrie() 
    {
        Node* current = root;
        current->deleteAllNodes(); 
        delete root;
    }
    // getters and setters
    Node* getPrefixNode(std::string s);
    // helpers 
    void buildTrie(SEXP x); 
    void addWord(std::string s);
    bool findWord(std::string s);
    // print 
    SEXP printTrie(SEXP x); 

private:
    Node* root;
};
