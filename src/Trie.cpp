#include "Trie.h"


using namespace Rcpp;

// FIXME: default NULL
SEXP Trie::printTrie(SEXP x)
{
    std::string s = Rcpp::as< std::string >(x);
    string_vec res;
    string_vec * res_ptr;
    res_ptr = &res;

    Node* current = root;
    if (s.size() == 0) {
        current->printNode("", res_ptr);
    } else { 
        current = getPrefixNode(s);
        if (current != 0)
            current->printNode(s, res_ptr);
    }

    return(Rcpp::wrap(res));
}

Node* Trie::getPrefixNode(std::string s)
{
    Node* current = root;
    for ( int i = 0; i < s.length(); i++ )
    {
        Node* tmp = current->findChild(s[i]);
        //std::cout << "Investigating node " << current << std::endl;
        if ( tmp == NULL )
            return tmp;
       // std::cout << "Child at " << tmp << ": " 
       //           << tmp->getValue() << std::endl; 
        current = tmp;
    }
    return current;
}

void Trie::addWord(std::string s)
{
    Node* current = root;
    //std::cout << "Root node: " << current << std::endl;
    if ( s.length() == 0 )
    {
        current->setWordMarker(); // an empty word
        return;
    }
    //std::cout << "Inserting word: " << s << std::endl;
    for ( int i = 0; i < s.length(); i++ )
    {
        Node* child = current->findChild(s[i]);
        if ( child != NULL )
        {
            current = child;
        }
        else
        {
            Node* tmp = new Node();
            tmp->setValue(s[i]);
            //std::cout << "New link from " << current << " to " 
            //          << tmp << " at "
            //          << s[i] << std::endl;

            current->appendChild(tmp);
            current = tmp;
        }
        if ( i == s.length() - 1 )
            current->setWordMarker();
    }
}

bool Trie::findWord(std::string s)
{
    Node* current = root;
    while ( current != NULL )
    {
        for ( int i = 0; i < s.length(); i++ )
        {
            Node* tmp = current->findChild(s[i]);
            if ( tmp == NULL )
                return false;
            current = tmp;
        }

        if ( current->getWordMarker() )
            return true;
        else
            return false;
    }
    return false;
}

void Trie::buildTrie(SEXP x)
{
    string_vec args = Rcpp::as< string_vec >(x);
    std::vector<std::string>::const_iterator itr;

    for(itr = args.begin(); itr != args.end(); itr++)
        addWord(*itr);
}

RCPP_MODULE(TrieModule){
    using namespace Rcpp;

    class_<Trie>( "Trie" )
    .constructor()
    .method( "buildTrie", &Trie::buildTrie, 
        "Construct a Trie from a vector of strings" )
    .method( "printTrie", &Trie::printTrie,
        "Print all words in the Trie with the given prefix" )
    .method( "deleteTrie", &Trie::deleteTrie,
        "Delete Trie and all child nodes" )
    ;
}
