use macroquad::prelude::*;

use brogle_core::{Bitboard, File, Rank, Tile};

fn draw_centered_text(text: &str, x: f32, y: f32, font_size: f32, color: Color) {
    let center = get_text_center(text, None, font_size as u16, 1.0, 0.0);
    draw_text(text, x - center.x, y - center.y, font_size, color);
}

fn parse_args() -> Bitboard {
    let mut args = std::env::args().skip(1);

    if args.len() < 1 {
        Bitboard::default()
    } else {
        let bit_string = args.next().unwrap().to_lowercase();
        let radix = if bit_string.starts_with("0x") || bit_string.len() == 16 {
            16
        } else {
            2
        };
        let bit_string = bit_string.trim_start_matches("0x");
        let bits = u64::from_str_radix(bit_string, radix)
            .expect("Expected argument of hexadecimal or binary number");

        Bitboard::new(bits)
    }
}

#[macroquad::main("BitBoard Viewer")]
async fn main() {
    // Get a BitBoard to render
    let mut board = parse_args();
    println!("\nBitBoard:\n{board}");

    loop {
        // Compute all necessary coordinates and values
        let (center_x, center_y) = (screen_width() / 2., screen_height() / 2.);
        let tile_size = screen_height().min(screen_width()) / 16.0;
        let start_x = center_x - (tile_size * 4.0);
        let start_y = center_y + (tile_size * 3.0);
        let text_size = tile_size / 2.0;
        let text_x = center_x;
        let text_y = screen_height() / 12.0;

        // Reset the canvas
        clear_background(GRAY);

        // Draw a chessboard
        for tile in Tile::iter() {
            let x = start_x + tile.file().index() as f32 * tile_size;
            let y = start_y - tile.rank().index() as f32 * tile_size;
            let text_color = if tile.is_light() { BLACK } else { WHITE };

            // Select tile color based on whether the tile is selected
            let color = if tile.is_light() {
                if board[tile] {
                    GREEN
                } else {
                    BEIGE
                }
            } else if board[tile] {
                DARKGREEN
            } else {
                DARKBROWN
            };

            // Draw the tile, then it's label
            draw_rectangle(x, y, tile_size, tile_size, color);
            draw_text(
                tile.to_string().as_str(),
                x + text_size / 2.0,
                y + text_size + text_size / 4.0,
                text_size,
                text_color,
            );
        }

        // If the mouse is clicked, select/deselect the tile clicked
        if is_mouse_button_pressed(MouseButton::Left) {
            let (mouse_x, mouse_y) = mouse_position();
            let tile_x = ((mouse_x - start_x) / tile_size).floor();
            let tile_y = ((start_y - mouse_y + tile_size) / tile_size).floor();

            // If a valid tile was clicked, toggle it
            if let (Ok(file), Ok(rank)) = (File::new(tile_x as u8), Rank::new(tile_y as u8)) {
                let tile = Tile::new(file, rank);
                board.toggle_tile(tile);
                println!("\nBitBoard:\n{board}");
            }
        }

        // Now display the binary and hex values of the BitBoard
        let binary_board = format!("{:#b}", board);
        let hex_board = format!("{:X}", board);
        draw_centered_text(
            binary_board.as_str(),
            text_x,
            text_y + text_size * 23.0,
            text_size * 1.5,
            WHITE,
        );
        draw_centered_text(
            hex_board.as_str(),
            text_x,
            text_y + text_size * 25.0,
            text_size * 2.0,
            WHITE,
        );

        next_frame().await;
    }
}
