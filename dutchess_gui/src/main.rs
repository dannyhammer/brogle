use dutchess_engine::game::ChessBoard as Board;
use macroquad::prelude::*;

const FILES: [char; 8] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];

#[macroquad::main("DUTChess")]
async fn main() {
    let board = Board::default();

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

    let mut selected_tile = None;
    loop {
        // TODO: Should probably only recompute when necessary...
        let tile_size = screen_height() / 10.0;
        let x = screen_width() / 2.0 - (tile_size * 4.0);
        let y = screen_height() / 2.0 - (tile_size * 4.0);
        let font_size = tile_size / 3.0;

        clear_background(LIGHTGRAY);

        draw_chessboard(&board, &icons, x, y, tile_size, font_size, selected_tile);

        if let Some(key) = get_last_key_pressed() {
            println!("{key:?}");
        }

        let (mouse_x, mouse_y) = mouse_position();

        if is_mouse_button_down(MouseButton::Left) {
            // println!("({mouse_x}, {mouse_y})");
            selected_tile = mouse_to_tile(mouse_x, mouse_y, x, y, tile_size);
            if let Some((file, rank)) = selected_tile {
                println!("Clicked {}{}", FILES[file], rank);
            }
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
) -> Option<(usize, usize)> {
    let board_size = tile_size * 8.0;

    let file = (mouse_x > board_x && mouse_x < board_x + board_size)
        .then(|| ((mouse_x - board_x) / tile_size) as usize)?;

    let rank = (mouse_y > board_y && mouse_y < board_y + board_size)
        .then(|| 8 - ((mouse_y - board_y) / tile_size) as usize)?;

    Some((file, rank))
}

fn draw_chessboard(
    board: &Board,
    icons: &[Texture2D],
    start_x: f32,
    start_y: f32,
    tile_size: f32,
    font_size: f32,
    selected_tile: Option<(usize, usize)>,
) {
    let tile_half = tile_size / 2.0;
    let board_size = tile_size * 8.0;
    let font_offset = font_size / 4.0;

    // File and Rank labels
    for i in 0..8 {
        let rank = format!("{}", 8 - i);
        let rank_y = start_y + tile_size * i as f32 + tile_half + font_offset;
        let rank_x_left = start_x - tile_half - font_offset;
        let rank_x_right = start_x + board_size + tile_half - font_offset;
        draw_text(rank.as_str(), rank_x_left, rank_y, font_size, BLACK);
        draw_text(rank.as_str(), rank_x_right, rank_y, font_size, BLACK);

        let file = format!("{}", FILES[i]);
        let file_x = start_x + tile_size * i as f32 + tile_half - font_offset;
        let file_y_bottom = start_y + board_size + tile_half + font_offset;
        let file_y_top = start_y - tile_half + font_offset;
        draw_text(file.as_str(), file_x, file_y_bottom, font_size, BLACK);
        draw_text(file.as_str(), file_x, file_y_top, font_size, BLACK);
    }

    // All tiles
    for tile in 0..64 {
        let file = tile % 8;
        let rank = tile / 8;

        let x = start_x + file as f32 * tile_size;
        let y = start_y + rank as f32 * tile_size;
        let (tile_color, text_color) = if (file + rank) % 2 == 0 {
            (BEIGE, BLACK)
        } else {
            (DARKBROWN, WHITE)
        };

        draw_rectangle(x, y, tile_size, tile_size, tile_color);
        let name = format!("{}{}", FILES[file], 8 - rank);
        draw_text(name.as_str(), x, y + tile_size, font_size, text_color);

        for i in 0..board.state.white.len() {
            if board.state.white[i].contains(&tile) {
                draw_texture(icons[i], x, y, WHITE);
            } else if board.state.black[i].contains(&tile) {
                draw_texture(icons[i], x, y, BLACK);
            }
        }
    }

    if let Some((file, rank)) = selected_tile {
        let tile_color = BROWN;
        let text_color = WHITE;

        let x = start_x + file as f32 * tile_size;
        let y = start_y + (8 - rank) as f32 * tile_size;

        draw_rectangle(x, y, tile_size, tile_size, tile_color);
        let name = format!("{}{}", FILES[file], 8 - rank);
        draw_text(name.as_str(), x, y + tile_size, font_size, text_color);
    }
}
