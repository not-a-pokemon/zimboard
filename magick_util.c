#include <MagickWand/MagickWand.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>

int zi_init(void)
{
	MagickWandGenesis();
	return 0;
}

int zi_uninit(void)
{
	MagickWandTerminus();
	return 0;
}

int zi_make_clean_blob(const uint8_t *buf, size_t buf_len, size_t *new_len, uint8_t **new_buf)
{
	MagickWand *wand = NULL;
	int status = -1;
	*new_buf = NULL;
	wand = NewMagickWand();
	if (wand == NULL)
		goto cleanup;
	if (MagickReadImageBlob(wand, buf, buf_len) == MagickFalse)
		goto cleanup;
	if ((*new_buf = MagickGetImageBlob(wand, new_len)) == NULL)
		goto cleanup;
	status = 0;
cleanup:
	if (wand != NULL)
		DestroyMagickWand(wand);
	return status;
}

void zi_free_blob(uint8_t *buf)
{
	MagickRelinquishMemory(buf);
}

int zi_make_thumbnail(const uint8_t *buf, size_t buf_len, int want_w, int want_h, const char *out_path)
{
	MagickWand *wand = NULL;
	int status = -1;
	wand = NewMagickWand();
	if (wand == NULL)
		goto cleanup;
	if (MagickReadImageBlob(wand, buf, buf_len) == MagickFalse)
		goto cleanup;
	if (MagickThumbnailImage(wand, want_w, want_h) == MagickFalse)
		goto cleanup;
	if (MagickWriteImage(wand, out_path) == MagickFalse)
		goto cleanup;
	status = 0;
cleanup:
	if (wand != NULL)
		DestroyMagickWand(wand);
	return status;
}

#if 0
int main(void) {
	zi_init();
	FILE *f = fopen("/home/timon/memes/0pints.jpg", "rb");
	char *t = malloc(200000);
	size_t t_ = 0;
	int c;
	while ((c = fgetc(f)) != EOF)
		t[t_++] = c;
	zi_make_thumbnail(t, t_, 128, 128, "./downscale.jpg");
	zi_uninit();
}
#endif
