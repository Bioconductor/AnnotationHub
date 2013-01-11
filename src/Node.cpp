#include "Node.h" 


Node* Node::findChild(char c) 
{
    for ( int i = 0; i < children.size(); i++ )
    {
        Node* tmp = children.at(i);
        if ( tmp->getValue() == c )
            return tmp;
    }
    return NULL;
}

void Node::printNode(std::string s, string_vec * res_ptr) 
{

    if ( getWordMarker() ) {
          (*res_ptr).push_back(s);
    }
    for ( int i = 0; i < children.size(); i++ ) {
            Node* tmp = children.at(i);
            if (tmp->getValue() != 0)
                tmp->printNode(s + tmp->getValue(), res_ptr);
    }
}
