#include <Rcpp.h>

using namespace std;
typedef std::vector<std::string> string_vec;

class Node {
public:
    // constructor, destructor
    Node()
    {
        value = ' ';
        word = false;
        parent = NULL;
        cumwords = 0;
    }
    void deleteAllNodes()
    {
        for (unsigned int i = 0; i < children.size(); i++)
        {
            Node* tmp = children.at(i);
            if (tmp->getWord() && tmp->children.size() == 0) {
                delete tmp;
            } else {
                tmp->deleteAllNodes();
            }
        }
    }
    // getters, setters
    std::string getValue() { return value; }
    void setValue(std::string v) { value = v; }
    bool getWord() { return word; }
    void setWord(bool w) { word = w; }
    int getCumwords() { return cumwords; }
    void setCumwords(int c) { cumwords = c; }
    Node* getParent() { return parent; }
    void setParent(Node* p) { parent = p; }
    std::vector<Node*> getChildren() { return children; }

    // build
    void appendChild(Node* child);
    Node* findChild(char c);

    // compress
    void compressNode();
    int computeCumwords();

    // print
    void printNodeDFS(std::string s, string_vec* res_ptr);
    void printNodeBFS(std::string s, string_vec* res_ptr, unsigned int max);
    void recordResult(string_vec* res_ptr);
    Node* findPrefixChild(std::string s);
    Node* findPrefixNode(std::string s);

private:
    std::string value;
    bool word;
    int cumwords;
    Node* parent;
    std::vector<Node*> children;
};
