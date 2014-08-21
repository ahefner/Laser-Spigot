#ifndef APP_TEXT_RENDER_H
#define APP_TEXT_RENDER_H

#include <stdint.h>
#include "c-util.h"

struct image
{
    int w, h, pitch, x_origin, y_origin;
    uint32_t *pixels;
};

typedef struct image *image_t;
image_t render_label_8 (unsigned facenum, uint32_t color, unsigned text_height, unsigned char *string);
image_t render_label_32 (unsigned facenum, uint32_t color, unsigned text_height, unsigned *string);
image_t render_glyph(unsigned facenum, uint32_t color, unsigned text_height, unsigned glyph);

#endif
