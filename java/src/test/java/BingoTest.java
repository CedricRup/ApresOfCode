import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Set;
import java.util.stream.IntStream;

public class BingoTest {

    String gridAsText = """
            22 13 17 11  0
            8  2 23  4 24
            21  9 14 16  7
            6 10  3 18  5
            1 12 20 15 19""";

    @Test
    public void glop(){

        IntStream draw = IntStream.of(22,8,21,6,1);
        var grids = Set.of(new Grid(gridAsText));
        int finalScore = Bingo.glop(draw,grids);
        Assertions.assertEquals(242, finalScore);
    }

    @Test
    public void glop1(){

        IntStream draw = IntStream.of(22,8,21,1,6);
        var grids = Set.of(new Grid(gridAsText));
        int finalScore = Bingo.glop(draw,grids);
        Assertions.assertEquals(1452, finalScore);
    }

}
