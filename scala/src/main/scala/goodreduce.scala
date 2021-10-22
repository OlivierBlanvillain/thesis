val img_batch = random_normal(#:(25, #:(256, #:(256, #:(3, Ø)))))
// start section goodreducescala
val avg_colors = mean(
  img_batch, #:(3, #:(1, #:(2, Ø))))
// end section goodreducescala
val avg_color_square: NDArray[Float, 5 #: 5 #: Ø] = reshape(avg_colors, #:(5, #:(5, Ø)))

// SHOULD NOT COMPILE!! Uncomment
// val wrong: NDArray[Float, 25 #: Ø.type] = mean(img_batch, #:(0, #:(1, #:(2, Ø))))
