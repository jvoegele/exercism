import java.util.NoSuchElementException;

public class SimpleLinkedList {
  private static class Node {

    private int data;
    private Node next;

    public Node() {
    }

    public Node(int data) {
      this.data = data;
    }

    public Node(int data, Node next) {
      this.data = data;
      this.next = next;
    }

    public int getData() {
      return data;
    }

    public void setData(int data) {
      this.data = data;
    }

    public Node getNext() {
      return next;
    }

    public void setNext(Node next) {
      this.next = next;
    }
  }

  private Node head;

  public SimpleLinkedList() {
  }

  public SimpleLinkedList(Integer[] values) {
    Node prev = null;
    for (Integer i : values) {
      Node current = new Node(i);
      if (head == null)
        head = current;
      if (prev != null) {
        prev.setNext(current);
      }
      prev = current;
    }
  }

  public int size() {
    int result = 0;
    Node current = head;
    while (current != null) {
      current = current.next;
      result++;
    }
    return result;
  }

  public int pop() {
    if (this.head == null) {
      throw new NoSuchElementException();
    }
    int result = this.head.getData();
    this.head = this.head.getNext();
    return result;
  }

  public void push(int i) {
    Node node = new Node(i, this.head);
    this.head = node;
  }

  public void reverse() {
    Node current = this.head;
    Node prev = null;
    Node next = null;

    while (current != null) {
      next = current.getNext();
      current.setNext(prev);
      prev = current;
      current = next;
    }
    this.head = prev;
  }

  public int[] asArray(Class klass) {
    return null;
  }
}
