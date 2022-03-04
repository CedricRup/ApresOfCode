public class GridElement {
  private boolean marked = false;
  private int number;

  public GridElement(int number){
      this.number = number;
  }

  public GridElement(String s)  {
    this(Integer.parseInt(s));
  }


    public boolean isMarked() {
        return marked;
    }

    public int getNumber() {
        return number;
    }

    public void mark() {
      marked = true;
    }
}
