#pragma once
#include <string>
#include <functional>

class BST {
public:
    using KeyType = int;
    using ItemType = std::string;

    ItemType* lookup(KeyType);
    void insert(KeyType, ItemType);
    void remove(KeyType);
    void removeIf(std::function<bool(KeyType)>);
    void displayEntries();
    void displayTree();

    BST(const BST &);
    BST & operator=(const BST &);
    BST(BST &&);
    BST & operator=(BST &&);
    BST() = default;
    ~BST();

private:
    struct Node;
    Node* root = leaf();
    static Node* leaf();

    static bool isLeaf(Node*);
    ItemType* lookupRec(KeyType, Node*);
    void insertRec(KeyType, ItemType, Node*&);
    Node* removeRec(KeyType, Node*&);
    void displayEntriesRec(Node*);
    void displayTreeRec(Node*, int = 0);
    Node* detachMinimumNode(Node*&);
    void deepDelete(Node*);
    Node* deepCopy(Node*);
};
