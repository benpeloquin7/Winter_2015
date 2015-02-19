#see http://web.stanford.edu/~bpeloqui/HW3-DAG.png 
flip = function(p) runif(1) < p
rain.sim = function(i) {
  Zeus.angry = flip(.3)
  Zeus.crying = flip(.2)
  thunder = (Zeus.angry && flip(.7)) || flip(.3)
  rain = (Zeus.angry && flip(.8)) || (Zeus.crying && flip(.99))
  forgetful.gardner = flip(.4)
  sprinkler = forgetful.gardner && flip(.7)
  wet.grass = (rain && flip(.8)) || (sprinkler && flip(.9)) || flip(.1)
  return (c(Zeus.angry = Zeus.angry, Zeus.crying = Zeus.crying,
  thunder = thunder, forgetful.gardner = forgetful.gardner, rain = rain,
  sprinkler = sprinkler, wet.grass = wet.grass))
}
