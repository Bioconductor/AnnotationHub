#include "Node.h"
#include <queue>

using namespace std;

Node* Node::findChild(char c) 
{
    for (unsigned int i = 0; i < children.size(); i++) {
        Node* tmp = children.at(i);
        if (tmp->getValue() == c)
            return tmp;
    }
    return NULL;
}

// breadth-first search
void Node::printNodeBFS(std::string s, string_vec* res_ptr, unsigned int max)
{
    // check incoming
    if (getWordMarker()) {
          (*res_ptr).push_back(s);
    }
    // load incoming
    std::queue<Node*> q;
    q.push(this);

    unsigned int counter = 0;
    unsigned int size = 0;
    while (counter < max && (size = q.size()) > 0) {
        for (unsigned int i = 0; i < size; i++) {
            Node* current = q.front();
            // add children
            if (current->children.size() > 0) {
                for (unsigned int i = 0; i < current->children.size(); i++)
                    q.push(current->children[i]);
            }
            // add words
            if (current->getWordMarker()) {
                std::string newstring;
                while ((current->getParent()) != NULL) {
                    newstring = current->getValue() + newstring;
                    current = current->getParent();
                }
                (*res_ptr).push_back(newstring);
                counter++;
            }
            q.pop();
        }
    }
}

// depth-first search
void Node::printNodeDFS(std::string s, string_vec* res_ptr)
{
    if (getWordMarker()) {
          (*res_ptr).push_back(s);
    }
    for (unsigned int i = 0; i < children.size(); i++) {
            Node* tmp = children.at(i);
            if (tmp->getValue() != 0)
                tmp->printNodeDFS(s + tmp->getValue(), res_ptr);
    }
}

