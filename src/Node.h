#include <Rcpp.h>

typedef std::vector<std::string> string_vec;

class Node {
public:
    // constructor, deconstructor 
    Node() { value = ' '; wordMarker = false; }
    void deleteAllNodes()
    {
        for ( int i = 0; i < children.size(); i++ )
        {
            Node* tmp = children.at(i);
            // no children and wordMarker == TRUE 
            if ( tmp->getWordMarker() && tmp->children.size() == 0 ) {
                delete tmp;
            } else {
                tmp->deleteAllNodes();
            }
        }
    }
    // getters and setters
    char getValue() { return value; }
    void setValue(char c) { value = c; }
    bool getWordMarker() { return wordMarker; }
    void setWordMarker() { wordMarker = true; }
    std::vector<Node*> getChildren() { return children; }
    // helpers
    void appendChild(Node* child) { children.push_back(child); }
    Node* findChild(char c);
    // print
    void printNode(std::string s, string_vec * res_ptr);

private:
    char value;
    bool wordMarker;
    std::vector<Node*> children;
};
