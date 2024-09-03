/*
use brogle_core::*;
use brogle_engine::*;
use macroquad::prelude::*;

#[derive(Default)]
struct GUI {
    selected: Option<Square>,
    highlighted: BitBoard,
    engine: Engine,
}

#[macroquad::main("DUTChess")]
async fn main() {
    // let x: u32 = 16384;
    // println!("LEFT -1: {}", x << -1);
    // println!("RIGHT 1: {}", x >> 1);

    let mut board = GUI::default();
    println!("{}", board.engine);

    let mut icons = [Texture2D::empty(); 12];
    let load = |path| Texture2D::from_file_with_format(path, None);
    icons[0] = load(include_bytes!("../assets/pawn_white.png"));
    icons[1] = load(include_bytes!("../assets/knight_white.png"));
    icons[2] = load(include_bytes!("../assets/bishop_white.png"));
    icons[3] = load(include_bytes!("../assets/rook_white.png"));
    icons[4] = load(include_bytes!("../assets/queen_white.png"));
    icons[5] = load(include_bytes!("../assets/king_white.png"));

    icons[6] = load(include_bytes!("../assets/pawn_black.png"));
    icons[7] = load(include_bytes!("../assets/knight_black.png"));
    icons[8] = load(include_bytes!("../assets/bishop_black.png"));
    icons[9] = load(include_bytes!("../assets/rook_black.png"));
    icons[10] = load(include_bytes!("../assets/queen_black.png"));
    icons[11] = load(include_bytes!("../assets/king_black.png"));

    loop {
        // TODO: Should probably only recompute when necessary...
        let square_size = screen_height() / 10.0;
        let x = (screen_width() / 2.0) - (square_size * 4.0);
        let y = (screen_height() / 2.0) - (square_size * 4.0);
        let font_size = square_size / 3.0;

        clear_background(LIGHTGRAY);

        draw_chessboard(&board, &icons, x, y, square_size);
        draw_ui(&board, x, y, square_size, font_size);

        // if let Some(key) = get_last_key_pressed() {
        //     println!("{key:?}");
        // }
        let (mouse_x, mouse_y) = mouse_position();

        if is_mouse_button_pressed(MouseButton::Right) {
            board.selected = None;
            // let selected = mouse_to_square(mouse_x, mouse_y, x, y, square_size);
            // if selected == board.selected {
            // }
        }

        if is_mouse_button_pressed(MouseButton::Left) {
            board.selected = mouse_to_square(mouse_x, mouse_y, x, y, square_size);
            if let Some(square) = board.selected {
                println!("Clicked {square}");
                board.highlighted = board.engine.legal_moves_of(square);
            }
        }
        /*
        if is_mouse_button_pressed(MouseButton::Left) {
            let prev = board.selected;
            let curr = mouse_to_square(mouse_x, mouse_y, x, y, square_size);

            // If the user clicked a square, we need to check if they want to move a piece.
            if let Some(curr_square) = curr {
                // print!("Current: {curr_square}, ");

                // If there was a previously-selected square, we need to move a piece!
                if let Some(prev_square) = prev {
                    // print!("Previous: {prev_square}, ");

                    // let legality = board.board.legality(prev_square, curr_square);
                    // if legality.is_legal() {
                    if board.board.is_legal(prev_square, curr_square) {
                        if board.board.make_move(prev_square, curr_square) {
                            // Move was made, so de-select everything
                            board.selected = None;
                        } else {
                            // Move was not made, so leave selection
                            board.selected = curr;
                            if let Some(piece) = board.board.piece_at(curr_square) {
                                board.highlighted = board.board.legal_moves_of(piece);
                            }
                        }
                    } else {
                        // Move was not legal to make
                        // println!("{legality}");
                        println!("Move is not legal!");
                        board.selected = None;
                    }

                    // // If there was a piece on that square, we need to move it
                    // if let Some(piece) = board.board.clear(prev_square) {
                    //     // println!("Moving {piece} from {prev_square} to {curr_square}");
                    //     board.board.set(curr_square, piece);
                    //     board.selected = None;
                    // } else {
                    //     // Since the prev square had no piece, we mark this as the new selection
                    //     // println!("Prev square {prev_square} had no piece");
                    //     board.selected = curr;
                    // }
                } else {
                    // No new square was selected, but we have a previous selection
                    // println!("Currently selected {curr_square}, but no previous selection exists");
                    board.selected = curr;
                    if let Some(piece) = board.board.piece_at(curr_square) {
                        board.highlighted = board.board.legal_moves_of(piece);
                        println!("{}", board.highlighted);
                    }
                }
            } else {
                // println!("There was no current selection");
            }

            // if let Some(square) = board.selected {
            //     if let Some(piece) = board.board.get(square) {
            //         println!("Selected {piece}");
            //     } else {
            //         println!("Clicked {square}");
            //     }
            // }
        }
         */
        /*
        if is_mouse_button_down(MouseButton::Left) {
            // board.selected = mouse_to_square(mouse_x, mouse_y, x, y, square_size);
            // Drag and drop
            if let Some(square) = board.selected {
                if let Some(piece) = board.board.get(square) {
                    println!("Picking up {piece}");
                    // pick_up_piece();
                    // board.board.remove(square);
                }
            }
        }
         */
        if is_mouse_button_released(MouseButton::Left) {
            // board.selected = None;
        }

        next_frame().await
    }
}

fn mouse_to_square(
    mouse_x: f32,
    mouse_y: f32,
    board_x: f32,
    board_y: f32,
    square_size: f32,
) -> Option<Square> {
    let board_size = square_size * 8.0;

    // println!("X: {mouse_x}, {board_x}, {board_size}");
    // println!("Y: {mouse_y}, {board_y}, {board_size}");
    // Files are the x axis
    let file = (mouse_x > board_x && mouse_x < board_x + board_size)
        .then(|| ((mouse_x - board_x) / square_size) as u8)?;

    // Ranks are the y axis
    // We subtract from 7 here because ranks go in "descending" order
    // Equivalent to subtracting by 8 then adding 1 for the start-at-one offset
    let rank = (mouse_y > board_y && mouse_y < board_size + board_y)
        .then(|| 7 - ((mouse_y - board_y) / square_size) as u8)?;

    // println!("Clicking FILE({file}), RANK({rank})");
    let file = File::new(file).ok()?;
    let rank = Rank::new(rank).ok()?;

    // println!("{file}{rank}");

    Some(Square::new(file, rank))
}

fn draw_chessboard(board: &GUI, icons: &[Texture2D], start_x: f32, start_y: f32, square_size: f32) {
    // We start drawing at the bottom of the board and work our way up
    let start_y = start_y + square_size * 7.0;

    for square in Square::iter() {
        let x = start_x + square.file().index() as f32 * square_size;
        let y = start_y - square.rank().index() as f32 * square_size;

        let mut square_color = if square.is_light() { BEIGE } else { DARKBROWN };

        if let Some(selected) = board.selected {
            if square == selected {
                if board.engine.piece_at(selected).is_some() {
                    square_color = SKYBLUE;
                } else {
                    square_color = YELLOW;
                }
            }
        }

        if board.highlighted.get(square) {
            square_color = if square.is_light() { LIGHTGRAY } else { DARKGRAY };
        }

        // let r = square.file().index() as f32 / 8.0;
        // let g = square.rank().index() as f32 / 8.0;
        // let b = (r + g) * 0.5;
        // let square_color = macroquad::color::Color::new(r, g, b, 1.0);
        draw_rectangle(x, y, square_size, square_size, square_color);

        if let Some(piece) = board.engine.piece_at(square) {
            draw_texture(icons[piece.index()], x, y, WHITE);
        }
    }
}

// fn draw_square(x: f32, y: f32, size: f32, color: macroquad::color::Color, piece: Option<Piece>) {
//     draw_rectangle(x, y, size, size, color);
//     if let Some(piece) = piece {
//         draw_texture(icons[piece.index()], x, y, WHITE);
//     }
// }

fn draw_ui(board: &GUI, start_x: f32, start_y: f32, square_size: f32, font_size: f32) {
    let square_half = square_size / 2.0;
    let board_size = square_size * 8.0;
    let font_offset = font_size / 4.0;
    // We start drawing at the bottom of the board and work our way up
    let start_y = start_y + square_size * 7.0;

    let fen = board.engine.fen();
    draw_text(fen.as_str(), font_offset, font_size, font_size, BLACK);

    // File and Rank labels
    for i in 0..8 {
        let rank = format!("{}", i + 1);
        let rank_y = start_y - square_size * i as f32 + square_half + font_offset;
        let rank_x_left = start_x - square_half - font_offset;
        let rank_x_right = start_x + board_size + square_half - font_offset;
        draw_text(rank.as_str(), rank_x_left, rank_y, font_size, BLACK);
        draw_text(rank.as_str(), rank_x_right, rank_y, font_size, BLACK);

        let file = format!("{}", File::new(i).unwrap());
        let file_x = start_x + square_size * i as f32 + square_half - font_offset;
        let file_y_top = start_y - board_size + square_half + font_offset;
        let file_y_bottom = start_y + square_size + square_half + font_offset;
        draw_text(file.as_str(), file_x, file_y_top, font_size, BLACK);
        draw_text(file.as_str(), file_x, file_y_bottom, font_size, BLACK);
    }

    for square in Square::iter() {
        let x = start_x + square.file().index() as f32 * square_size + font_offset;
        let y = start_y - square.rank().index() as f32 * square_size + square_size - font_offset;
        let color = if square.is_light() { BLACK } else { WHITE };

        let square_id = format!("{}", square.index());
        draw_text(&square_id, x, y, font_size, color);
    }
}

 */
fn main() {}
