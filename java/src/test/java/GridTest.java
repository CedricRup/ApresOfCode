import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class GridTest {

    String gridAsText = """
            22 13 17 11  0
            8  2 23  4 24
            21  9 14 16  7
            6 10  3 18  5
            1 12 20 15 19""";

    @Test
    public void gridCanMarkANumber() {
        Grid grid = new Grid(gridAsText);
        grid.mark(8);
        Assertions.assertTrue(grid.isMarked(0, 1));
    }

    @Test
    public void initiallyElementIsNotMarked() {
        Grid grid = new Grid(gridAsText);
        Assertions.assertFalse(grid.isMarked(0, 1));
    }

    @Test
    public void initialScoreIsSumOfElement() {
        Grid grid = new Grid(gridAsText);
        var score = grid.score();
        Assertions.assertEquals(300, score);
    }

    @Test
    public void scoreDoesntCountMarkedNumber() {
        Grid grid = new Grid(gridAsText);
        grid.mark(8);
        var score = grid.score();
        Assertions.assertEquals(292, score);
    }

    @Test
    public void scoreDoesntChangeWhenMarkingANotPresentNumber() {
        Grid grid = new Grid(gridAsText);
        grid.mark(999);
        var score = grid.score();
        Assertions.assertEquals(300, score);
    }

    @Test
    public void WinIfFirstLineIsAllMarked() {
        Grid grid = new Grid(gridAsText);

        grid.mark(22);
        grid.mark(13);
        grid.mark(17);
        grid.mark(11);
        grid.mark(0);

        Assertions.assertTrue(grid.isWining());
    }

    @Test
    public void NotWinningIfANumberIsNotMarkedInALine() {
        Grid grid = new Grid(gridAsText);

        grid.mark(22);
        grid.mark(13);
        grid.mark(17);
        grid.mark(11);
        Assertions.assertFalse(grid.isWining());
    }

    @Test
    public void WinAnyLineIsAllMarked() {
        Grid grid = new Grid(gridAsText);

        grid.mark(1);
        grid.mark(12);
        grid.mark(20);
        grid.mark(15);
        grid.mark(19);

        Assertions.assertTrue(grid.isWining());
    }

    @Test
    public void WinFirstColumnIsAllMarked() {
        Grid grid = new Grid(gridAsText);

        grid.mark(22);
        grid.mark(8);
        grid.mark(21);
        grid.mark(6);
        grid.mark(1);

        Assertions.assertTrue(grid.isWining());
    }

    @Test
    public void WinAnyColumnIsAllMarked() {
        Grid grid = new Grid(gridAsText);

        grid.mark(0);
        grid.mark(24);
        grid.mark(7);
        grid.mark(5);
        grid.mark(19);

        Assertions.assertTrue(grid.isWining());
    }
}

