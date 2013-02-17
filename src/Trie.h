#include <Rcpp.h>
#include "Node.h"

using namespace Rcpp;

class Trie {
public:
    // constructor, destructor
    Trie() { root = new Node(); }
    Trie(SEXP x) {
	root = new Node();
	buildTrie(x);
    }
    ~Trie() 
    {
        Node* current = root;
        current->deleteAllNodes(); 
        delete root;
    }

    // build
    void buildTrie(CharacterVector x);
    void addWord(std::string word);
    Node* findNode(std::string s);

    // compress
    void compressTrie();

    // print
    CharacterVector printTrie();
    CharacterVector printTrie(CharacterVector x, unsigned int max);

private:
    Node* root;
};
