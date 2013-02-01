#include <Rcpp.h>

using namespace std;
typedef std::vector<std::string> string_vec;

class Node {
public:
    // constructor, destructor
    Node() {value = ' '; wordMarker = false;}
    void deleteAllNodes()
    {
        for (unsigned int i = 0; i < children.size(); i++)
        {
            Node* tmp = children.at(i);
            if (tmp->getWordMarker() && tmp->children.size() == 0) {
                delete tmp;
            } else {
                tmp->deleteAllNodes();
            }
        }
    }
    // getters, setters
    char getValue() {return value;}
    void setValue(char c) {value = c;}
    bool getWordMarker() {return wordMarker;}
    void setWordMarker() {wordMarker = true;}
    Node* getParent() {return parent;}
    void setParent(Node* p) {parent = p;}
    // search
    std::vector<Node*> getChildren() {return children;}
    void appendChild(Node* child) {children.push_back(child);}
    Node* findChild(char c);
    // print
    void printNodeDFS(std::string s, string_vec* res_ptr);
    void printNodeBFS(std::string s, string_vec* res_ptr, unsigned int max);

private:
    char value;
    bool wordMarker;
    Node* parent;
    std::vector<Node*> children;
};
