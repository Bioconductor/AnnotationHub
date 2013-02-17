#include "Node.h"
#include <queue>
#include <sstream>

using namespace std;

// **************************************
// build
// **************************************
Node* Node::findChild(char c)
{
    for (unsigned int i = 0; i < children.size(); i++) {
        Node* tmp = children.at(i);
        if (tmp->getValue()[0] == c)
            return tmp;
    }
    return NULL;
}

void Node::appendChild(Node* child)
{
    children.push_back(child);
}

// **************************************
// compression
// **************************************
// compression and 'cumwords' assignment
void Node::compressNode()
{
    //Node* current = this;
    if (children.size() == 0) {
        // no children
        return;
    } else if (children.size() == 1) {
        // single child: combine nodes if current is not a word
        Node* child = children.at(0);
        if (!getWord()) {
            setValue(getValue() + child->getValue());
            children = child->children;
            setWord(child->getWord());
            setCumwords(getCumwords() + child->getCumwords());
            compressNode();
        } else {
            child->compressNode();
            setCumwords(getCumwords() + child->getCumwords());
        }
    } else {
        // multiple children
        for (unsigned int i = 0; i < children.size(); i++) {
            Node* child = getChildren().at(i);
            child->compressNode();
        }
        setCumwords(computeCumwords());
    }
}

// sums completion values for all child nodes
int Node::computeCumwords()
{
    int count = 0;
    for (unsigned int i = 0; i < children.size(); i++)
        count = count + getChildren()[i]->getCumwords();
    return count;
}

// **************************************
// print prefixes
// **************************************
Node* Node::findPrefixNode(std::string s)
{
    Node* current = this;
    Node* tmp = current->findPrefixChild(s);
    if (tmp == NULL) {
        return tmp;
    } else {
        current = tmp;
        current->findPrefixNode(s);
    }
    return current;
}

// Checks if supplied prefix is contained in any children
Node* Node::findPrefixChild(std::string s)
{
    for (unsigned int i = 0; i < children.size(); i++) {
        Node* tmp = children.at(i);
        if ((tmp->getValue().find(s) != std::string::npos))
            return tmp;
    }
    return NULL;
}

void Node::printNodeBFS(std::string s, string_vec *res_ptr, unsigned int max)
{
    // add incoming to the queue
    std::queue<Node*> q;
    q.push(this);

    // fill the queue <= max
    unsigned int counter = 0;
    while (!q.empty() && (counter + q.size()) <= max) {
        Node* current = q.front();
        // if current is word, add to result
        if (current->getWord()) {
            current->recordResult(res_ptr);
            counter++;
        }
        // add children to queue and remove parent
        if (q.size() <= max) {
            for (unsigned int i = 0; i < current->children.size(); i++)
                q.push(current->children[i]);
        }
        q.pop();
    }
    // print the queue to the result
    while (!q.empty()) {
        Node* current = q.front();
        current->recordResult(res_ptr);
        q.pop();
    }
}

// Print a node to the result
void Node::recordResult(string_vec* res_ptr)
{
    Node* current = this;
    std::ostringstream ostr;
    ostr << current->cumwords;
    std::string nwords = ostr.str();
    std::string newstring;
    bool word = current->getWord();
    while ((current->getParent()) != NULL) {
        newstring = current->getValue() + newstring;
        current = current->getParent();
    }
    if (word)
        (*res_ptr).push_back(newstring);
    else
        (*res_ptr).push_back(newstring + "...[" + nwords + "]");
}

// **************************************
// print full words
// **************************************
// Print all words in Trie via depth-first search
void Node::printNodeDFS(std::string s, string_vec *res_ptr)
{
    if (getWord()) {
          (*res_ptr).push_back(s);
    }
    for (unsigned int i = 0; i < children.size(); i++) {
            Node* tmp = children.at(i);
            if (!tmp->getValue().empty())
                tmp->printNodeDFS(s + tmp->getValue(), res_ptr);
    }
}

