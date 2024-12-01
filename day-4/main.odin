package main

import "core:fmt"
import "core:os"
import "core:strings"

Grid :: struct {
	rows:    []string,
	columns: []string,
	all:     string,
}

exists :: proc(grid: Grid) -> bool {
	return grid.rows != nil && grid.columns != nil
}

cleanup :: proc(grid: Grid) {
	delete(grid.rows)
	delete(grid.columns)
}

parse_grid :: proc(filename: string) -> Grid {
	data, ok := os.read_entire_file_from_filename(filename)
	if !ok {
		return Grid{nil, nil, ""}
	}

	str := strings.trim(string(data), "\n")
	rows := strings.split_lines(str)
	str, _ = strings.remove_all(str, "\n")

	column_len := len(rows[0])
	row_len := len(rows)
	columns := make([]string, row_len)
	for &column, col_i in columns {
		builder := strings.builder_make_len(column_len)
		for row in rows {
			strings.write_byte(&builder, row[col_i])
		}
		col := strings.to_string(builder)
		column = col
	}

	return Grid{rows, columns, str}
}

main :: proc() {
	grid := parse_grid("input.txt")
	defer cleanup(grid)

	part_one(grid)
	part_two(grid)
}

part_one :: proc(grid: Grid) {
	total_xmas := 0
	for column in grid.columns {
		total_xmas += strings.count(column, "XMAS") + strings.count(column, "SAMX")
	}

	for row in grid.rows {
		total_xmas += strings.count(row, "XMAS") + strings.count(row, "SAMX")
	}

	builder := strings.builder_make_len(4)
	defer strings.builder_destroy(&builder)

	row_backtrack := [4]int{0, 0, 0, 0}
	for _, x in grid.all {
		row_backtrack = [4]int{0, 0, 0, 0}
		strings.builder_reset(&builder)
		for i in 0 ..< 4 {
			base := (i * (len(grid.columns) + 1))
			target := base + x
			row_backtrack[i] = target / len(grid.columns)
			if target >= len(grid.all) {
				break
			}
			if i > 0 && abs(row_backtrack[i] - row_backtrack[i - 1]) != 1 {
				break
			}
			strings.write_byte(&builder, grid.all[target])
		}
		as_str := strings.to_string(builder)
		total_xmas += strings.count(as_str, "XMAS") + strings.count(as_str, "SAMX")
	}

	for _, x in grid.all {
		row_backtrack = [4]int{0, 0, 0, 0}
		strings.builder_reset(&builder)
		for i in 0 ..< 4 {
			base := (i * (len(grid.columns) - 1))
			target := base + x
			row_backtrack[i] = target / len(grid.columns)
			if target < 0 || target >= len(grid.all) {
				break
			}
			if i > 0 && abs(row_backtrack[i] - row_backtrack[i - 1]) != 1 {
				break
			}
			strings.write_byte(&builder, grid.all[target])
		}
		as_str := strings.to_string(builder)
		total_xmas += strings.count(as_str, "XMAS") + strings.count(as_str, "SAMX")
	}
	fmt.printfln("Total: {:d}", total_xmas)
}

part_two :: proc(grid: Grid) {
	total_xmas := 0

	builder_l := strings.builder_make_len(3)
	defer strings.builder_destroy(&builder_l)
	builder_r := strings.builder_make_len(3)
	defer strings.builder_destroy(&builder_r)

	for y in 1 ..< (len(grid.rows) - 1) {
		for x in 1 ..< (len(grid.columns) - 1) {
			strings.builder_reset(&builder_l)
			strings.builder_reset(&builder_r)
			for step in 0 ..< 3 {
				target_l_row := y - 1 + step // row from top to bottom
				target_l_col := x - 1 + step // column from left to right

				target_r_row := y - 1 + step // same but for the right side
				target_r_col := x + 1 - step

				if target_l_row < 0 || target_l_col < 0 { 	// it's a square, no need for other checks
					break
				}
				if target_r_row < 0 || target_r_col < 0 {
					break
				}

				char_l := grid.rows[target_l_row][target_l_col]
				char_r := grid.rows[target_r_row][target_r_col]

				strings.write_byte(&builder_l, char_l)
				strings.write_byte(&builder_r, char_r)
			}

			as_str_l := strings.to_string(builder_l)
			as_str_r := strings.to_string(builder_r)

			left_match := strings.count(as_str_l, "MAS") + strings.count(as_str_l, "SAM")
			right_match := strings.count(as_str_r, "MAS") + strings.count(as_str_r, "SAM")

			// any amtch -> win
			if left_match > 0 && right_match > 0 {
				total_xmas += 1
			}
		}
	}

	fmt.printfln("Total MAS: {:d}", total_xmas)
}

