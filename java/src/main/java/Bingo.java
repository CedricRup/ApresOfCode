import java.util.Set;
import java.util.stream.IntStream;

public class Bingo {
    public static int glop(IntStream draw, Set<Grid> grids) {
        var grid = grids.stream().findFirst().get();
        var drawAsArray = draw.toArray();
        for(var i : drawAsArray){
            grid.mark(i);
            if (grid.isWining()){
                return grid.score() * i;
            }
        }
        return 0;
    }
}
