use dutchess_engine::*;
use macroquad::prelude::*;

#[derive(Default)]
struct BoardGUI {
    selected: Option<Position>,
    highlighted: BitBoard,
    board: ChessBoard,
}

#[macroquad::main("DUTChess")]
async fn main() {
    // let x: u32 = 16384;
    // println!("LEFT -1: {}", x << -1);
    // println!("RIGHT 1: {}", x >> 1);

    let mut board = BoardGUI::default();
    println!("{}", board.board);

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
        let tile_size = screen_height() / 10.0;
        let x = screen_width() / 2.0 - (tile_size * 4.0);
        let y = screen_height() / 2.0 - (tile_size * 4.0);
        let font_size = tile_size / 3.0;

        clear_background(LIGHTGRAY);

        draw_chessboard(&board, &icons, x, y, tile_size);
        draw_ui(&board, x, y, tile_size, font_size);

        // if let Some(key) = get_last_key_pressed() {
        //     println!("{key:?}");
        // }
        let (mouse_x, mouse_y) = mouse_position();

        if is_mouse_button_pressed(MouseButton::Right) {
            board.selected = None;
            // let selected = mouse_to_tile(mouse_x, mouse_y, x, y, tile_size);
            // if selected == board.selected {
            // }
        }

        if is_mouse_button_pressed(MouseButton::Left) {
            board.selected = mouse_to_tile(mouse_x, mouse_y, x, y, tile_size);
            if let Some(tile) = board.selected {
                if let Some(piece) = board.board.piece_at(tile) {
                    board.highlighted = board.board.legal_moves_of(piece, tile);
                }
            }
        }
        /*
        if is_mouse_button_pressed(MouseButton::Left) {
            let prev = board.selected;
            let curr = mouse_to_tile(mouse_x, mouse_y, x, y, tile_size);

            // If the user clicked a tile, we need to check if they want to move a piece.
            if let Some(curr_tile) = curr {
                // print!("Current: {curr_tile}, ");

                // If there was a previously-selected tile, we need to move a piece!
                if let Some(prev_tile) = prev {
                    // print!("Previous: {prev_tile}, ");

                    // let legality = board.board.legality(prev_tile, curr_tile);
                    // if legality.is_legal() {
                    if board.board.is_legal(prev_tile, curr_tile) {
                        if board.board.make_move(prev_tile, curr_tile) {
                            // Move was made, so de-select everything
                            board.selected = None;
                        } else {
                            // Move was not made, so leave selection
                            board.selected = curr;
                            if let Some(piece) = board.board.piece_at(curr_tile) {
                                board.highlighted = board.board.legal_moves_of(piece);
                            }
                        }
                    } else {
                        // Move was not legal to make
                        // println!("{legality}");
                        println!("Move is not legal!");
                        board.selected = None;
                    }

                    // // If there was a piece on that tile, we need to move it
                    // if let Some(piece) = board.board.clear(prev_tile) {
                    //     // println!("Moving {piece} from {prev_tile} to {curr_tile}");
                    //     board.board.set(curr_tile, piece);
                    //     board.selected = None;
                    // } else {
                    //     // Since the prev tile had no piece, we mark this as the new selection
                    //     // println!("Prev tile {prev_tile} had no piece");
                    //     board.selected = curr;
                    // }
                } else {
                    // No new tile was selected, but we have a previous selection
                    // println!("Currently selected {curr_tile}, but no previous selection exists");
                    board.selected = curr;
                    if let Some(piece) = board.board.piece_at(curr_tile) {
                        board.highlighted = board.board.legal_moves_of(piece);
                        println!("{}", board.highlighted);
                    }
                }
            } else {
                // println!("There was no current selection");
            }

            // if let Some(tile) = board.selected {
            //     if let Some(piece) = board.board.get(tile) {
            //         println!("Selected {piece}");
            //     } else {
            //         println!("Clicked {tile}");
            //     }
            // }
        }
         */
        /*
        if is_mouse_button_down(MouseButton::Left) {
            // board.selected = mouse_to_tile(mouse_x, mouse_y, x, y, tile_size);
            // Drag and drop
            if let Some(tile) = board.selected {
                if let Some(piece) = board.board.get(tile) {
                    println!("Picking up {piece}");
                    // pick_up_piece();
                    // board.board.remove(tile);
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

fn mouse_to_tile(
    mouse_x: f32,
    mouse_y: f32,
    board_x: f32,
    board_y: f32,
    tile_size: f32,
) -> Option<Position> {
    let board_size = tile_size * 8.0;

    let file = (mouse_x > board_x && mouse_x < board_x + board_size)
        .then(|| ((mouse_x - board_x) / tile_size) as u8)?;

    let rank = (mouse_y > board_y && mouse_y < board_y + board_size)
        .then(|| ((mouse_y - board_y) / tile_size) as u8)?;

    let file = File::new(file).unwrap();
    let rank = Rank::new(rank).unwrap();

    Some(Position::new(file, rank))
}

fn draw_chessboard(
    board: &BoardGUI,
    icons: &[Texture2D],
    start_x: f32,
    start_y: f32,
    tile_size: f32,
) {
    // All tiles
    for tile in Position::iter() {
        let x = start_x + tile.rank().index() as f32 * tile_size;
        let y = start_y + tile.file().index() as f32 * tile_size;

        let mut tile_color = if tile.is_light() { BEIGE } else { DARKBROWN };

        if let Some(selected) = board.selected {
            // if let Some(piece) = board.board.piece_at(selected) {
            if board.board.piece_at(selected).is_some() {
                if tile == selected {
                    tile_color = SKYBLUE;
                }
            }
        }

        if board.highlighted.get(tile) {
            tile_color = if tile.is_light() { LIGHTGRAY } else { DARKGRAY };
        }

        draw_rectangle(x, y, tile_size, tile_size, tile_color);

        if let Some(piece) = board.board.piece_at(tile) {
            draw_texture(icons[piece.index()], x, y, WHITE);
        }
    }
}

fn draw_ui(board: &BoardGUI, start_x: f32, start_y: f32, tile_size: f32, font_size: f32) {
    let tile_half = tile_size / 2.0;
    let board_size = tile_size * 8.0;
    let font_offset = font_size / 4.0;

    let fen = board.board.fen();
    draw_text(fen.as_str(), font_offset, font_size, font_size, BLACK);

    // File and Rank labels
    for i in 0..8 {
        let rank = format!("{}", 8 - i);
        let rank_y = start_y + tile_size * i as f32 + tile_half + font_offset;
        let rank_x_left = start_x - tile_half - font_offset;
        let rank_x_right = start_x + board_size + tile_half - font_offset;
        draw_text(rank.as_str(), rank_x_left, rank_y, font_size, BLACK);
        draw_text(rank.as_str(), rank_x_right, rank_y, font_size, BLACK);

        let file = format!("{}", File::new(i).unwrap().to_string());
        let file_x = start_x + tile_size * i as f32 + tile_half - font_offset;
        let file_y_bottom = start_y + board_size + tile_half + font_offset;
        let file_y_top = start_y - tile_half + font_offset;
        draw_text(file.as_str(), file_x, file_y_bottom, font_size, BLACK);
        draw_text(file.as_str(), file_x, file_y_top, font_size, BLACK);
    }
}
