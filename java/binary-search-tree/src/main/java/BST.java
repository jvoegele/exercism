import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class BST<T extends Comparable<T>> {
  public static class Node<T extends Comparable<T>> {
    private final T data;
    private Node<T> left, right;

    private Node(T data) {
      this.data = data;
    }

    private Node(T data, Node<T> left, Node<T> right) {
      this.data = data;
      this.left = left;
      this.right = right;
    }

    public T getData() {
      return data;
    }

    public Node<T> getLeft() {
      return left;
    }

    private void setLeft(Node<T> left) {
      this.left = left;
    }

    public Node<T> getRight() {
      return right;
    }

    private void setRight(Node<T> right) {
      this.right = right;
    }
  }

  private Node<T> root;

  public Node<T> getRoot() {
    return root;
  }

  public void insert(T data) {
    if (root == null) {
      root = new Node(data);
    }
    else {
      Node<T> node = new Node<T>(data);
      insertNode(node, root);
    }
  }

  public List<T> getAsLevelOrderList() {
    List<T> result = new ArrayList<>();
    Queue<Node<T>> queue = new LinkedList<>();
    queue.add(this.getRoot());
    while (!queue.isEmpty()) {
      Node<T> node = queue.remove();
      if (node != null) {
        result.add(node.getData());
        queue.add(node.getLeft());
        queue.add(node.getRight());
      }
    }
    return result;
  }

  public List<T> getAsSortedList() {
    List<T> result = new ArrayList<>();
    buildSortedList(this.getRoot(), result);
    return result;
  }

  private void buildSortedList(Node<T> node, List<T> acc) {
    if (node != null) {
      buildSortedList(node.getLeft(), acc);
      acc.add(node.getData());
      buildSortedList(node.getRight(), acc);
    }
  }

  private void insertNode(Node<T> newNode, Node<T> currentNode) {
    Node<T> nextNode = null;
    boolean insertLeft;
    if (newNode.getData().compareTo(currentNode.getData()) <= 0) {
      nextNode = currentNode.getLeft();
      insertLeft = true;
    }
    else {
      nextNode = currentNode.getRight();
      insertLeft = false;
    }

    if (nextNode == null) {
      if (insertLeft) {
        currentNode.setLeft(newNode);
      }
      else {
        currentNode.setRight(newNode);
      }
    }
    else {
      insertNode(newNode, nextNode);
    }
  }
}
