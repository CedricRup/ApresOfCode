import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

public class Grid {

    private static final Predicate<GridElement> IS_MARKED = GridElement::isMarked;
    List<List<GridElement>> state;

    public Grid(String gridAsText) {
        state = new ArrayList<>();
        for (String line : gridAsText.split("\n")) {
            state.add(
                    Stream.of(line.split("\s"))
                            .filter(s -> !s.isBlank())
                            .map(GridElement::new)
                            .toList());
        }
    }

    public void mark(int numberToMark) {
        state.stream()
                .flatMap(Collection::stream)
                .filter(gridElement -> gridElement.getNumber() == numberToMark)
                .findFirst().ifPresent(GridElement::mark);
    }

    public boolean isMarked(int column, int row) {
        return state.get(row).get(column).isMarked();
    }

    public int score() {
        return state.stream().flatMap(Collection::stream).filter(IS_MARKED.negate()).mapToInt(GridElement::getNumber).sum();
    }

    public boolean isWining() {
        return checkColumns() || checkLines();
    }

    private boolean checkLines() {
        return state
                .stream()
                .anyMatch(line -> line.stream().allMatch(GridElement::isMarked));
    }

    private boolean checkColumns() {
        for (var column = 0; column < state.size(); column++) {
            if (checkColumn(column)) return true;
        }
        return false;
    }

    private boolean checkColumn(int column) {
        var hasAllColumnMarked = true;

        for (var gridElements : state) {
            hasAllColumnMarked = hasAllColumnMarked && gridElements.get(column).isMarked();
        }
        return hasAllColumnMarked;
    }
}
