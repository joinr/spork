(ns spork.graphics2d.swing.shared
  (:import  [java.awt Transparency Color]
            [java.awt.image BufferedImage ImageObserver]
            [javax.imageio ImageIO]
            [javax.swing JFrame]))

(def opaque Transparency/OPAQUE)
(def +clear+ (Color. 255 255 255 0))

;;These are duplicated in image....trying to eliminate the dependency though.
(def null-observer 
  (reify ImageObserver 
     (imageUpdate [this img infoflags x y width height]
       false)))

(def get-transparency 
  (let [transmap {:opaque Transparency/OPAQUE
                  :translucent Transparency/TRANSLUCENT
                  :bitmask Transparency/BITMASK
                  Transparency/OPAQUE Transparency/OPAQUE
                  Transparency/TRANSLUCENT Transparency/TRANSLUCENT
                  Transparency/BITMASK Transparency/BITMASK}]
    (fn [t]
      (get transmap t Transparency/OPAQUE))))

