do -- custom event listeners
  event = {}
  local events = {}

  local function Error(e,level)
    player.chat(e,0xFF0000)
    error(e, (level or 1) + 1)
  end

  function event.registerEvent(name)
    if events[name] then
      Error("Attempted to register event by name \"" .. name .. "\" when it was already registered",2)
    end
    events[name] = {}
  end

  function event.addListener(name,funct)
    if events[name] then
      events[name][funct] = true
    else
      Error("Attempted to add listener to non-existant event by name \"" .. name .. "\"",2)
    end
  end

  function event.removeListener(name,funct)
    if events[name] then
      events[name][funct] = nil
    else
      Error("Attempted to add listener to non-existant event by name \"" .. name .. "\"",2)
    end
  end

  function event.doEvent(name)
    if events[name] then
      for i in pairs(events[name]) do
        i()
      end
    else
      Error("Attempted to do non-existant event by name \"" .. name .. "\"",2)
    end
  end


end

do -- other utilities

  utils = {}

  -- localize some functions
  local floor = math.floor

  --[[
    Converts a triple of RGB values to a hex color value.
     @param r
       Red value in range [0,1]
     @param g
       Green value in range [0,1]
     @param b
       Blue value in range [0,1]
     @return
       0xRRGGBB value corresponding to the given (r,g,b) triple.
  ]]--
  function utils.RGBtoHex(r,g,b) 
    local rL = floor(r*0xff + 0.5)
    local gL = floor(g*0xff + 0.5)
    local bL = floor(b*0xff + 0.5)
    return bL + gL*0x100 + rL*0x10000
  end

  --[[[
    Converts a hex color value to a table of RGB values
      @param hex
        Color value in format 0xRRGGBB
      @return
        Table of RGB values in format {red, green, blue}
  ]]--
  function utils.HexToRGB(hex)
    local b = hex%0x100
    local g = (hex - b)/0x100 % 0x100
    local r = (hex - g - b)/0x10000 % 0x100
    return {r/0xFF,g/0xFF,b/0xFF}
  end

  -- linear interpolation: b*m + (1-m)*a
  local function lerp(a,b,m)
    return b*m + (1-m)*a
  end

  math.lerp = lerp

  --[[
    Linear interpolation from one list to another.
    @param a
      List of numbers
    @param b
      List of numbers
    @requires
      size(a) = size(b)
    @returns
      List of numbers where 
        list[i] = lerp(a[i],b[i],m)
  ]]--
  function utils.listLerp(a,b,m)
    local result = {}
    for i=1,#a do
      result[i] = lerp(a[i],b[i],m)
    end
    return result
  end

  --[[
    Linear interpolation from one list to another. 
    @param a
      List of numbers
    @param b
      List of numbers
    @param p
      Power
    @requires
      size(a) = size(b)
    @returns
      List of numbers where 
        list[i] = lerp(a[i]^p,b[i]^p,m)^(1/p)
  ]]--
  function utils.listLerpPow(a,b,m,p)
    local result = {}
    for i=1,#a do
      result[i] = lerp(a[i]^p,b[i]^p,m)^(1/p)
    end
    return result
  end
  --[[
    Averages a list of RGB colors
    @param list
      A list of RGB colors in the format {{R_1,G_1,B_1},...,{R_n,G_n,B_n}}
    @returns
      An RGB color in the format {R,G,B} that is the average of the colors.
  ]]--
  function utils.averageRGB(list,pow)
    local p = pow or 2
    local r, g, b = 0, 0, 0
    local n = #list
    for i=1,n do
      local c = list[i]
      r,g,b = r + c[1]^p, g + c[2]^p, b + c[3]^p
    end
    return {(r/n)^(1/p),(g/n)^(1/p),(b/n)^(1/p)}
  end

  --[[
    Shuffles a list in place.
  ]]--
  function utils.shuffle(list)
    local rand = math.random
    for i=#list,1,-1 do
      local r = rand(1,i)
      if r ~= i then
        list[r], list[i] = list[i], list[r]
      end
    end
  end

end

local iCount = 200

local gradient = {} -- starts at [0]
local gradientRGB = {} -- starts at [0], has {R,G,B} tables for colors
local gradientSmooth = {} -- starts at [0], has 12x the entries
local gradientSmoothRGB = {} -- starts at [0], has 12x the entries
local getGradientColorAt

do -- generate gradient
  local gradControlPoints = {
    [0]={0.03,0.03,0.15}, -- the [0]= is necessary.
    {0.1,0.3,0.7}, --red, green, blue.
    {0.7,0.7,0.8},
    {0.9,0.6,0.3},
    {0.6,0.2,0.1},
    {0.03,0.02,0.06}
  }
  local gradDouble = {}
  local gamma = 1

  gradControlPoints[#gradControlPoints + 1] = gradControlPoints[0]
  local sSize = 360/#gradControlPoints
  for i=0,359 do
    local cLow = math.floor(i/sSize)
    local cHigh = math.ceil(i/sSize)
    local sPos = (i%sSize)/sSize
    local Color = utils.listLerpPow(gradControlPoints[cLow],gradControlPoints[cHigh],sPos,gamma)
    gradDouble[i] = Color
    gradientRGB[i] = Color
    gradient[i] = utils.RGBtoHex(Color[1],Color[2],Color[3])
  end

  sSize = (360*12)/#gradControlPoints
  for i=0,(360*12)-1 do
    local cLow = math.floor(i/sSize)
    local cHigh = math.ceil(i/sSize)
    local sPos = (i%sSize)/sSize
    local Color = utils.listLerpPow(gradControlPoints[cLow],gradControlPoints[cHigh],sPos,gamma)
    gradientSmoothRGB[i] = Color
    gradientSmooth[i] = utils.RGBtoHex(Color[1],Color[2],Color[3])
  end

  getGradientColorAt = function(i)
    if i ~= i%360 then i = i%360 end
    if i%1 == 0 then
      return gradient[i]
    else
      local cLow = math.floor(i)
      local cHigh = math.ceil(i)%360
      local sPos = i%1
      local c = utils.listLerpPow(gradDouble[cLow],gradDouble[cHigh],sPos,gamma)
      return utils.RGBtoHex(c[1],c[2],c[3])
    end
  end

  local CACHE = {}
  local floor = math.floor
  local function cacheFloor(i)
    return floor(i*24)
  end
  getGradientColorAtRGB = function(i)
    if i ~= i%360 then i = i%360 end
    local cacheVal = cacheFloor(i)
    if CACHE[cacheVal] then return CACHE[cacheVal] end
    if i%1 == 0 then
      CACHE[cacheVal] = gradientRGB[i]
      return gradientRGB[i]
    else
      local cLow = math.floor(i)
      local cHigh = math.ceil(i)%360
      local sPos = i%1
      local c = utils.listLerpPow(gradDouble[cLow],gradDouble[cHigh],sPos,gamma)
      CACHE[cacheVal] = c
      return c
    end
  end

end

--{"None", "Grid", "Random", "Jitter", "N-Rooks"}
local sampleGenerators = {
  ["None"] = function(n,c) return {{x = {}, y = {}}} end,
  ["Grid"] = function(n,c)
    local sqrtn = math.floor(math.sqrt(n)+0.5)
    local tab = {x = {}, y = {}} -- Offsets from center of a pixel
    local tabX, tabY = tab.x, tab.y
    local last = 0
    local const = (sqrtn+1) / (2*sqrtn)
    for x=1,sqrtn do 
      local thisX = x/sqrtn - const
      for y=1,sqrtn do
        local thisY = y/sqrtn - const
        last = last + 1
        tabX[last] = thisX
        tabY[last] = thisY
      end
    end
    local rTab = {}
    for i=1,c do
      rTab[i] = tab
    end
    return rTab
  end,
  ["Random"] = function(n,c)
    local rand = math.random
    local rTab = {}
    for i=1,c do
      rTab[i] = {x = {}, y = {}}
      local tabX = rTab[i].x
      local tabY = rTab[i].y
      for j = 1,n do
        tabX[j] = rand() - 0.5
        tabY[j] = rand() - 0.5
      end
    end
    return rTab
  end,
  ["Jitter"] = function(n,c)
    local sqrtn = math.floor(math.sqrt(n)+0.5)
    local rand = math.random
    local rTab = {}
    for i=1,c do
      local tab = {x = {}, y = {}} -- Offsets from center of a pixel
      local tabX, tabY = tab.x, tab.y
      local last = 0
      local const = (sqrtn+1) / (2*sqrtn)
      local rm = 1/sqrtn
      for x=1,sqrtn do 
        local thisX = x/sqrtn - const
        for y=1,sqrtn do
          local thisY = y/sqrtn - const
          last = last + 1
          tabX[last] = thisX + rm*(rand()-0.5)
          tabY[last] = thisY + rm*(rand()-0.5)
        end
      end
      rTab[i] = tab
    end
    return rTab
  end,
  ["N-Rooks"] = function(n,c)
    local rTab = {}
    for i=1,c do
      local tab = {x = {}, y = {}}
      local tabX, tabY = tab.x, tab.y
      local const = (n+1) / (2*n)
      for j=1,n do
        local value = j/n - const
        tabX[j],tabY[j] = value, value
      end
      utils.shuffle(tabX)
      utils.shuffle(tabY)
      rTab[i] = tab
    end
    return rTab
  end,
}


CO_VAR = { -- Variables needed by coroutines
  OP_LIMIT = 200, -- max "operations" per frame.
  ITER_PER_OP = 200, -- number of iterations to count as one operation
  RES_X = 675, -- 675 pixels across
  RES_Y = 480, -- 480 pixels down
  R_BASE = -2, -- low value real
  I_BASE = -64/45, -- low value imag
  SCALE = 4/675,
  ITER_MAX = 200,
  SMOOTH = false,
  ANTIALIASING = false,
  AA_Threshold = 0.05,
  AA_Supersample = 2, -- uses AA_Supersample^2 points for supersampling.
  ITER_MULT = 4, -- multiplier for iteration count in coloring.
  gradient = gradient,
  gradientRGB = gradientRGB,
  gradientSmooth = gradientSmooth,
  gradientSmoothRGB = gradientSmoothRGB,
  getGradientColorAt = getGradientColorAt,
  getGradientColorAtRGB = getGradientColorAtRGB,
}

-- A coroutine function that renders the mandelbrot set.
local function renderSet()
  local log = math.log
  local startTime = tolua(game.elapsedMS)
  local maxIterations = CO_VAR.ITER_MAX
  local setPixel = tolua(CO_VAR.layerBase.setPixel)
  local setRect = tolua(CO_VAR.layerBase.setRect)
  local opLimit = CO_VAR.OP_LIMIT
  local iterPerOp = CO_VAR.ITER_PER_OP
  local bailout = (128)^2
  local il, lp = 1/log(2), log(log(bailout^0.5))
  local ops = 0
  local smooth = CO_VAR.SMOOTH
  local AA = CO_VAR.ANTIALIASING
  local RGB_TAB = {}
  local iterMult = CO_VAR.ITER_MULT
  local HexToRGB = utils.HexToRGB
  local ZeroRGB = {0,0,0}
  for x = 0,CO_VAR.RES_X do
    local r = CO_VAR.R_BASE + x*CO_VAR.SCALE
    local colLast = -2
    local lY = -1 -- position itLast was first spotted at
    local RGB_column = {}
    RGB_TAB[x] = RGB_column
    for y = 0,CO_VAR.RES_Y do
      local i = CO_VAR.I_BASE + (CO_VAR.RES_Y-y)*CO_VAR.SCALE
      local iterations = 0
      local zr, zi, cr, ci = r, i, r, i
      --local z = new(r,i)
      --local c = new(r,i)
      while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
        --z = add(square(z),c)
        zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
        iterations = iterations + 1
      end
      local col = -1
      ops = ops + iterations/iterPerOp
      if (zr*zr + zi*zi) < bailout and iterations >= maxIterations then 
        iterations = -1 -- Infinity
      elseif not smooth then
        col = gradient[(iterations*iterMult)%360]
      elseif smooth then
        local abs = (zr*zr + zi*zi)^0.5
        local iterSmooth = iterations + il*lp - il*log(log(abs))
        col = getGradientColorAt((iterSmooth*iterMult)%360)
      end
      if AA then
        if col == -1 then 
          RGB_column[y] = ZeroRGB
        else
          if smooth then
            RGB_column[y] = getGradientColorAtRGB((iterSmooth*iterMult)%360)
          else
            RGB_column[y] = gradientRGB[(iterations*iterMult)%360]
          end
        end
      end
      if col ~= colLast or y == CO_VAR.RES_Y then
        if colLast >= 0 then -- Handle stuff above
          if y - lY > 1 then
            setRect(x,lY,1,y-lY,0xFF000000 + colLast)
            ops = ops + 1
          else
            setPixel(x,lY,0xFF000000 + colLast)
            ops = ops + 1
          end
        end
        colLast = col
        lY = y
      end

        --setPixel(x,y,0xFF000000 + gradient[iterations%360])
        --ops = ops + 1
      if ops > opLimit then
        ops = ops - opLimit
        coroutine.yield({ID = "status", x = x, y = y})
        opLimit = CO_VAR.OP_LIMIT
      end
    end
  end
  if CO_VAR.ANTIALIASING then
    -- stage 1: look for places to supersample
    local pointsX = {}
    local pointsY = {}
    local pointsLength = 0

    local threshold = CO_VAR.AA_Threshold
    local abs = math.abs
    local function compareRGB(c1,c2)
      if c1 == c2 then return false end
      return
        abs(c1[1]-c2[1]) > threshold or
        abs(c1[2]-c2[2]) > threshold or
        abs(c1[3]-c2[3]) > threshold
    end
    local min,max = math.min, math.max
    for x = 0,CO_VAR.RES_X do
      local RGB_column = RGB_TAB[x]
      local columnPrev = RGB_TAB[max(x-1,0)]
      local columnNext = RGB_TAB[min(x+1,CO_VAR.RES_X)]
      local yAt = RGB_column[0]
      local yPrev = yAt
      local yNext = RGB_column[1]
      local maxY = CO_VAR.RES_Y
      for y = 0,maxY do
        if compareRGB(yAt,yPrev) or
           compareRGB(yAt,yNext) or
           compareRGB(yAt,columnPrev[y]) or
           compareRGB(yAt,columnNext[y]) then
          pointsLength = pointsLength + 1
          pointsX[pointsLength] = x
          pointsY[pointsLength] = y
        end
        ops = ops + 1/8
        if ops > opLimit then
          ops = ops - opLimit
          coroutine.yield({ID = "AAstatus1",x = x, y = y})
          opLimit = CO_VAR.OP_LIMIT
        end
        yPrev = yAt
        yAt = yNext
        yNext = RGB_column[min(y+1,maxY)]
      end
      if x >= 1 then RGB_TAB[x-1] = nil end
    end
    RGB_TAB = nil

    local COPIES = 7 -- copies of the table to go through
    local method = CO_VAR.AA_Method
    local super = CO_VAR.AA_Supersample
    local pointTable = sampleGenerators[method](super,7)
    for index = 1, pointsLength do
      local x, y = pointsX[index], pointsY[index]
      local maxY = CO_VAR.RES_Y
      local red, g, b = 0, 0, 0
      local offsets = pointTable[index%COPIES + 1]
      local offx, offy = offsets.x, offsets.y
      for p = 1,super do
        local r = CO_VAR.R_BASE + (x + offx[p])*CO_VAR.SCALE
        local i = CO_VAR.I_BASE + (maxY-y + offy[p])*CO_VAR.SCALE
        local iterations = 0
        local zr, zi, cr, ci = r, i, r, i
        --local z = new(r,i)
        --local c = new(r,i)
        while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
          --z = add(square(z),c)
          zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
          iterations = iterations + 1
        end
        local col = ZeroRGB
        ops = ops + iterations/iterPerOp
        if (zr*zr + zi*zi) < bailout and iterations >= maxIterations then 
          iterations = -1 -- Infinity
        elseif not smooth then
          col = gradientRGB[(iterations*iterMult)%360]
        elseif smooth then
          local abs = (zr*zr + zi*zi)^0.5
          local iterSmooth = iterations + il*lp - il*log(log(abs))
          col = getGradientColorAtRGB((iterSmooth*iterMult)%360)
        end
        red, g, b = red + col[1]^2, g + col[2]^2, b + col[3]^2
        if ops > opLimit then
          ops = ops - opLimit
          coroutine.yield({ID = "AAstatus2",i = index,max = pointsLength})
          opLimit = CO_VAR.OP_LIMIT
        end
      end
      setPixel(x,y,0xFF000000 + utils.RGBtoHex((red/super) ^ (1/2),(g/super) ^ (1/2),(b/super) ^ (1/2)))
      ops = ops + 1
    end
  end
  coroutine.yield({ID = "finish", time = tolua(game.elapsedMS) - startTime})
end

local function renderSet2()
  local log = math.log
  local startTime = tolua(game.elapsedMS)
  local maxIterations = CO_VAR.ITER_MAX
  local setPixel = tolua(CO_VAR.layerBase.setPixel)
  local setRect = tolua(CO_VAR.layerBase.setRect)
  local opLimit = CO_VAR.OP_LIMIT
  local iterPerOp = CO_VAR.ITER_PER_OP
  local bailout = (128)^2
  local il, lp = 1/log(2), log(log(bailout^0.5))
  local ops = 0
  local smooth = CO_VAR.SMOOTH
  local AA = CO_VAR.ANTIALIASING
  local iterMult = CO_VAR.ITER_MULT
  local HexToRGB = utils.HexToRGB
  local ZeroRGB = {0,0,0}
  local patterns = {
    {}, -- No pattern
    { 1, 1,-7, 1, 0,-2, 1,-1,-8}, -- [2] left
    {-1, 1,-6,-1, 0,-3,-1,-1,-9}, -- [3] right
    {-1, 1,-6, 0, 1,-4, 1, 1,-7}, -- [4] top
    {-1,-1,-9, 0,-1,-5, 1,-1,-8}, -- [5] bottom
    {-1, 1,-6,-1, 0,-3,-1,-1,-9, 0, 1,-4, 1, 1,-7}, -- [6] top right
    { 1, 1,-7, 1, 0,-2, 1,-1,-8,-1, 1,-6, 0, 1,-4}, -- [7] top left
    { 1, 1,-7, 1, 0,-2, 1,-1,-8,-1,-1,-9, 0,-1,-5}, -- [8] bottom left
    {-1, 1,-6,-1, 0,-3,-1,-1,-9, 1,-1,-8, 0,-1,-5}, -- [9] bottom right
    {-1, 1,-6,-1, 0,-3, 0, 1,-4}, -- [10] top right
    { 1, 1,-7, 1, 0,-2, 0, 1,-4}, -- [11] top left
    { 1, 0,-2, 1,-1,-8, 0,-1,-5}, -- [12] bottom left
    {-1, 0,-3,-1,-1,-9, 0,-1,-5}, -- [13] bottom right
  }
  local renderTable = {}
  --[[
    type == "table" : {R,G,B} color
    -1 : Not initialized yet
    -2 : Initialized from left (Not yet run)
    -3 : Initialized from right (Not yet run)
    -4 : Initialized from above (Not yet run)
    -5 : Initialized from below (Not yet run)
    -6 : Initialized from top right (Not yet run)
    -7 : Initialized from top left (Not yet run)
    -8 : Initialized from bottom left (Not yet run)
    -9 : Initialized from bottom right (Not yet run)
    -10 : Initialized initially from top right (Not yet run)
    -11 : Initialized initially from top left (Not yet run)
    -12 : Initialized initially from bottom left (Not yet run)
    -13 : Initialized initially from bottom right (Not yet run)
  ]]--
  -- First, fill renderTable with non-initialized values
  for x = 0,CO_VAR.RES_X do
    local col = {}
    renderTable[x] = col
    for y = 0,CO_VAR.RES_Y do
      col[y] = -1
    end
    ops = ops + CO_VAR.RES_Y/100
    if ops > opLimit then
      ops = ops - opLimit
      coroutine.yield({ID = "preinit", x = x, y = y})
      opLimit = CO_VAR.OP_LIMIT
    end
  end
  local renderQueue = {}
  local rQ_Start = 1
  local rQ_End = 0 -- last element
  do -- Initialize edge values respectively
 
    local maxX = CO_VAR.RES_X
    local maxY = CO_VAR.RES_Y
    for x = 1,maxX-1 do
      renderTable[x][0] = -4 -- initialize top row "from above"
      renderQueue[rQ_End+1] = {x,0}
      renderTable[x][maxY] = -5 -- initialize bottom row "from below"
      renderQueue[rQ_End+2] = {x,maxY}
      rQ_End = rQ_End + 2
    end

    local col0,colm = renderTable[0], renderTable[maxX]
    for y = 1,maxY-1 do
      col0[y] = -2 -- initalize left column "from left"
      renderQueue[rQ_End+1] = {0,y}
      colm[y] = -3 -- initalize right column "from right"
      renderQueue[rQ_End+2] = {maxX,y}
      rQ_End = rQ_End + 2
    end

    -- Initialize corners
    renderTable[0][0] = -11
    renderQueue[rQ_End+1] = {0,0}
    renderTable[maxX][0] = -10
    renderQueue[rQ_End+2] = {maxX,0}
    renderTable[0][maxY] = -12
    renderQueue[rQ_End+3] = {0,maxY}
    renderTable[maxX][maxY] = -13
    renderQueue[rQ_End+4] = {maxX,maxY}
    rQ_End = rQ_End + 4
  end

  local min, max = math.min, math.max
  while rQ_End >= rQ_Start do
    local rqIndex = math.random(max(rQ_Start,rQ_End - 4),rQ_End)
    local point = renderQueue[rqIndex]
    renderQueue[rqIndex] = renderQueue[rQ_Start]
    renderQueue[rQ_Start] = nil
    rQ_Start = rQ_Start + 1
    local x,y = point[1],point[2]
    local r = CO_VAR.R_BASE + x*CO_VAR.SCALE
    local i = CO_VAR.I_BASE + (CO_VAR.RES_Y-y)*CO_VAR.SCALE
    local iterations = 0
    local zr, zi, cr, ci = r, i, r, i
    while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
      zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
      iterations = iterations + 1
    end
    local col = ZeroRGB
    local colDraw = 0x000000
    ops = ops + iterations/iterPerOp
    if (zr*zr + zi*zi) < bailout and iterations >= maxIterations then 
      iterations = -1 -- Infinity
    elseif not smooth then
      col = gradientRGB[(iterations*iterMult)%360]
      colDraw = gradient[(iterations*iterMult)%360]
    elseif smooth then
      local abs = (zr*zr + zi*zi)^0.5
      local iterSmooth = iterations + il*lp - il*log(log(abs))
      col = getGradientColorAtRGB((iterSmooth*iterMult)%360)
      colDraw = utils.RGBtoHex(col[1],col[2],col[3])
    end
    setPixel(x,y,0xFF000000 + colDraw)
    ops = ops + 1
    if ops > opLimit then
      ops = ops - opLimit
      coroutine.yield({ID = "statusFF", qs = rQ_Start, qe = rQ_End})
      opLimit = CO_VAR.OP_LIMIT
    end
    -- Initialize pixels
    if iterations ~= -1 then
      local pattern = patterns[-renderTable[x][y]]
      for i=1,#pattern,3 do
        local tx, ty = x + pattern[i],y + pattern[i+1] 
        if renderTable[tx][ty] == -1 then
          renderTable[tx][ty] = pattern[i+2]
          renderQueue[rQ_End+1] = {tx,ty}
          rQ_End = rQ_End + 1
        end
      end
    end
    renderTable[x][y] = col
  end
  if CO_VAR.ANTIALIASING then
    -- stage 1: look for places to supersample
    local pointsX = {}
    local pointsY = {}
    local pointsLength = 0

    local threshold = CO_VAR.AA_Threshold
    local abs = math.abs
    local function compareRGB(c1,c2)
      if c1 == c2 then return false end
      if c2 == -1 then return false end
      return
        abs(c1[1]-c2[1]) > threshold or
        abs(c1[2]-c2[2]) > threshold or
        abs(c1[3]-c2[3]) > threshold
    end
    local min,max = math.min, math.max
    for x = 0,CO_VAR.RES_X do
      local RGB_column = renderTable[x]
      local columnPrev = renderTable[max(x-1,0)]
      local columnNext = renderTable[min(x+1,CO_VAR.RES_X)]
      local yAt = RGB_column[0]
      local yPrev = yAt
      local yNext = RGB_column[1]
      local maxY = CO_VAR.RES_Y
      for y = 0,maxY do
        if yAt ~= -1 then
          if compareRGB(yAt,yPrev) or
             compareRGB(yAt,yNext) or
             compareRGB(yAt,columnPrev[y]) or
             compareRGB(yAt,columnNext[y]) then
            pointsLength = pointsLength + 1
            pointsX[pointsLength] = x
            pointsY[pointsLength] = y
          end
          ops = ops + 1/8
          if ops > opLimit then
            ops = ops - opLimit
            coroutine.yield({ID = "AAstatus1",x = x, y = y})
            opLimit = CO_VAR.OP_LIMIT
          end
        end
        yPrev = yAt
        yAt = yNext
        yNext = RGB_column[min(y+1,maxY)]
      end
      if x >= 1 then renderTable[x-1] = nil end
    end
    renderTable = nil


    local COPIES = 7 -- copies of the table to go through
    local method = CO_VAR.AA_Method
    local super = CO_VAR.AA_Supersample
    local pointTable = sampleGenerators[method](super,7)
    for index = 1, pointsLength do
      local x, y = pointsX[index], pointsY[index]
      local maxY = CO_VAR.RES_Y
      local red, g, b = 0, 0, 0
      local offsets = pointTable[index%COPIES + 1]
      local offx, offy = offsets.x, offsets.y
      for p = 1,super do
        local r = CO_VAR.R_BASE + (x + offx[p])*CO_VAR.SCALE
        local i = CO_VAR.I_BASE + (maxY-y + offy[p])*CO_VAR.SCALE
        local iterations = 0
        local zr, zi, cr, ci = r, i, r, i
        --local z = new(r,i)
        --local c = new(r,i)
        while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
          --z = add(square(z),c)
          zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
          iterations = iterations + 1
        end
        local col = ZeroRGB
        ops = ops + iterations/iterPerOp
        if (zr*zr + zi*zi) < bailout and iterations >= maxIterations then 
          iterations = -1 -- Infinity
        elseif not smooth then
          col = gradientRGB[(iterations*iterMult)%360]
        elseif smooth then
          local abs = (zr*zr + zi*zi)^0.5
          local iterSmooth = iterations + il*lp - il*log(log(abs))
          col = getGradientColorAtRGB((iterSmooth*iterMult)%360)
        end
        red, g, b = red + col[1]^2, g + col[2]^2, b + col[3]^2
        if ops > opLimit then
          ops = ops - opLimit
          coroutine.yield({ID = "AAstatus2",i = index,max = pointsLength})
          opLimit = CO_VAR.OP_LIMIT
        end
      end
      setPixel(x,y,0xFF000000 + utils.RGBtoHex((red/super) ^ (1/2),(g/super) ^ (1/2),(b/super) ^ (1/2)))
      ops = ops + 1
    end
  end
  coroutine.yield({ID = "finish", time = tolua(game.elapsedMS) - startTime})
end

local function renderSet3()
  local log = math.log
  local startTime = tolua(game.elapsedMS)
  local maxIterations = CO_VAR.ITER_MAX
  local setPixel = tolua(CO_VAR.layerBase.setPixel)
  local setRect = tolua(CO_VAR.layerBase.setRect)
  local drawSprite = tolua(CO_VAR.layerBase.drawSprite)
  local opLimit = CO_VAR.OP_LIMIT
  local iterPerOp = CO_VAR.ITER_PER_OP
  local bailout = (128)^2
  local il, lp = 1/log(2), log(log(bailout^0.5))
  local ops = 0
  local smooth = CO_VAR.SMOOTH
  local AA = CO_VAR.ANTIALIASING
  local iterMult = CO_VAR.ITER_MULT
  local HexToRGB = utils.HexToRGB
  local ZeroRGB = {0,0,0}
  local gradient = smooth and CO_VAR.gradientSmooth or CO_VAR.gradient
  local smoothMult = (smooth and 12 or 1)

  local DEBUG = false
  local RECT_PROCESS = 64
  local ITER_BLOCK_PROCESS = 5000*iterMult*smoothMult
  --[[ 
    will process the whole rectangle IF:
      1) area is less than RECT_PROCESS
      2) no corner has a higher iteration count than ITER_BLOCK_PROCESS
  ]]--
  

  local debugDrawR = tolua(game.level.newSprite())
  debugDrawR.lineStyle(0xFFFF0000,1,toobject{
    pixelHinting = true,
    scaleMode = "none",
    caps = "square",
    joints = "miter",
  })
  debugDrawR.drawRect(0,0,1,1)
  local debugDrawY = tolua(game.level.newSprite())
  debugDrawY.lineStyle(0xFFFFFF00,1,toobject{
    pixelHinting = true,
    scaleMode = "none",
    caps = "square",
    joints = "miter",
  })
  debugDrawY.drawRect(0,0,1,1)
 
  --[[
    if rectangle perimeter is all same iteration count then fill
    break rectangles apart immediately on reaching different color, return both to stack.
    would have negative impact from smoothing. i'll figure that out later

    other stuff: 
     - break rectangles apart if contains values on both sides of 0
     - break along the longer axis.
     - all rectangles will check cache before ever computing a value
  ]]--

  local maxX, maxY = CO_VAR.RES_X-1, CO_VAR.RES_Y-1

  local rectangleQueue = {{0,0,maxX,maxY}}
  local rq_S = 1 -- first one
  local rq_E = 1 -- last one

  local r_Base = CO_VAR.R_BASE
  local i_Base = CO_VAR.I_BASE
  local scale = CO_VAR.SCALE

  local iterCount = {}
  -- iterCount[x][y]
  for x=0,maxX do
    iterCount[x] = {}
  end

  local pixelsDone = 0

  local function doPixel(x,y)
    pixelsDone = pixelsDone + 1
    local r = r_Base + x*scale
    local i = i_Base + (maxY-y+1)*scale
    local iterations = 0
    local zr, zi, cr, ci = r, i, r, i
    while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
      zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
      iterations = iterations + 1
    end
    ops = ops + iterations/iterPerOp
    if ops > opLimit then
      ops = ops - opLimit
      coroutine.yield({ID = "statusRF", p = pixelsDone})
      opLimit = CO_VAR.OP_LIMIT
    end
    if iterations >= maxIterations and (zr*zr + zi*zi) < bailout then 
      iterations = -1
    else
      iterations = iterations*iterMult
    end
    if iterations >= 0 and smooth then
      local abs = (zr*zr + zi*zi)^0.5
      local iterSmooth = iterations + (il*lp - il*log(log(abs)))*iterMult
      iterations = math.floor(iterSmooth*smoothMult)
    end
    iterCount[x][y] = iterations
    return iterations
  end

  local P_LENGTH_BASE = 10
  local P_INC = 1/2
  local function doPixelPeriodicity(x,y)
    pixelsDone = pixelsDone + 1
    local r = r_Base + x*scale
    local i = i_Base + (maxY-y+1)*scale
    local iterations = 0
    local zr, zi, cr, ci = r, i, r, i
    local ozr, ozi = zr, zi
    local p = 0
    local P_LENGTH = P_LENGTH_BASE
    local brokeEarly = false
    while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
      zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
      iterations = iterations + 1
      if zr == ozr and zi == ozi then
        brokeEarly = true
        break
      end
      p = p + 1
      if p >= P_LENGTH then
        P_LENGTH = P_LENGTH + P_INC
        ozr, ozi = zr, zi
        p = 0
      end
    end
    ops = ops + iterations/iterPerOp
    if ops > opLimit then
      ops = ops - opLimit
      coroutine.yield({ID = "statusRF", p = pixelsDone})
      opLimit = CO_VAR.OP_LIMIT
    end
    if (iterations >= maxIterations and (zr*zr + zi*zi) < bailout) or brokeEarly then 
      iterations = -1
    else
      iterations = iterations*iterMult
    end
    if iterations >= 0 and smooth then
      local abs = (zr*zr + zi*zi)^0.5
      local iterSmooth = iterations + (il*lp - il*log(log(abs)))*iterMult
      iterations = math.floor(iterSmooth*smoothMult)
    end
    iterCount[x][y] = iterations
    return iterations
  end

  local function doRect(x1,y1,x2,y2,p)
    local pix = p and doPixelPeriodicity or doPixel
    for x = x1, x2 do
      for y = y1, y2 do
        local i = iterCount[x][y] or pix(x,y)
      end
    end
    if (x2-x1) > (y2-y1) then -- wider
      for y = y1,y2 do
        local lastIter = iterCount[x1][y]
        local thisIter = lastIter
        local lastC = gradient[(lastIter)%(360*smoothMult)]
        local firstSee = x1
        for x = x1+1,x2 do
          local i = iterCount[x][y]
          local c = gradient[(i)%(360*smoothMult)]
          if i == -1 then c = 0 end
          if c ~= lastC then
            if lastIter ~= -1 then
              setRect(firstSee,y,x-firstSee,1,0xFF000000 + lastC)
              ops = ops + 1
            end
            lastIter = i
            lastC = c
            firstSee = x
          end
        end
        if lastIter ~= -1 then
          local c = gradient[(lastIter)%(360*smoothMult)]
          setRect(firstSee,y,x2-firstSee+1,1,0xFF000000 + c)
          ops = ops + 1
        end
      end
    else -- taller
      for x = x1,x2 do
        local icCol = iterCount[x]
        local lastIter = icCol[y1]
        local lastC = gradient[(lastIter)%(360*smoothMult)]
        local firstSee = y1
        for y = y1+1,y2 do
          local i = icCol[y]
          local c = gradient[(i)%(360*smoothMult)]
          if i == -1 then c = 0 end
          if c ~= lastC then
            if lastIter ~= -1 then
              setRect(x,firstSee,1,y-firstSee,0xFF000000 + lastC)
              ops = ops + 1
            end
            lastIter = i
            lastC = c
            firstSee = y
          end
        end
        if lastIter ~= -1 then
          local c = gradient[(lastIter)%(360*smoothMult)]
          setRect(x,firstSee,1,y2-firstSee+1,0xFF000000 + c)
          ops = ops + 1
        end
      end
    end
  end

  local x_Zero,y_Zero = -r_Base/scale,-i_Base/scale -- whatever these are figure it out

  local function breakX(rect, x)
    local x1,y1,x2,y2 = unpack(rect)
    rectangleQueue[rq_E + 1] = {x1,y1,x,y2}
    rectangleQueue[rq_E + 2] = {x+1,y1,x2,y2}
    if DEBUG then
      drawSprite(debugDrawR,x1,y1,x-x1,y2-y1)
      drawSprite(debugDrawR,x+1,y1,x2-(x+1),y2-y1)
    end
    rq_E = rq_E + 2
  end

  local function breakY(rect, y)
    local x1,y1,x2,y2 = unpack(rect)
    rectangleQueue[rq_E + 1] = {x1,y1,x2,y}
    rectangleQueue[rq_E + 2] = {x1,y+1,x2,y2}
    if DEBUG then
      drawSprite(debugDrawR,x1,y1,x2-x1,y-y1)
      drawSprite(debugDrawR,x1,y+1,x2-x1,y2-(y+1))
    end
    rq_E = rq_E + 2
  end

  local floor = math.floor

  local modifiedMax = maxIterations * iterMult * smoothMult 
  local function processRectangle(rect)
    local x1,y1,x2,y2 = unpack(rect)
    if DEBUG then
      drawSprite(debugDrawY,x1,y1,x2-x1,y2-y1)
    end
    if x1 < x_Zero and x2 > x_Zero then
      breakX(rect,floor(x_Zero))
      return
    elseif y1 < y_Zero and y2 > y_Zero then
      breakY(rect,floor(y_Zero))
      return
    end
    local T1 = iterCount[x1][y1] or doPixelPeriodicity(x1,y1)
    local T2 = iterCount[x2][y1] or doPixelPeriodicity(x2,y1)
    local T3 = iterCount[x1][y2] or doPixelPeriodicity(x1,y2)
    local T4 = iterCount[x2][y2] or doPixelPeriodicity(x2,y2)
    local hasInf = (T1 == -1 or T2 == -1 or T3 == -1 or T4 == -1)
    local pix = hasInf and doPixelPeriodicity or doPixel

    if ((x2-x1+1)*(y2-y1+1)) <= RECT_PROCESS then
      -- decide to do it or not
      if (T1 > ITER_BLOCK_PROCESS or 
          T2 > ITER_BLOCK_PROCESS or
          T3 > ITER_BLOCK_PROCESS or
          T4 > ITER_BLOCK_PROCESS) or
         (modifiedMax > ITER_BLOCK_PROCESS and
           hasInf
         ) then
        -- do nothing
      else
        doRect(x1,y1,x2,y2,hasInf)
        return
      end
    end

    if (x2-x1) >= (y2-y1) then -- wider, or equal.
      if (T1 ~= T2) or (T1 ~= T3) or (T1 ~= T4) then
        breakX(rect,floor((x1+x2)/2))
        return
      end

      for x = x1+1,x2-1 do
        local a = iterCount[x][y1] or pix(x,y1)
        local b = iterCount[x][y2] or pix(x,y2)
        if (T1 ~= a) or (a ~= b) then
          breakX(rect,floor((x1+x2)/2))
          return
        end
      end

      local icx1, icx2 = iterCount[x1], iterCount[x2]
      for y = y1+1,y2-1 do
        local a = icx1[y] or pix(x1,y)
        local b = icx2[y] or pix(x2,y)
        if (T1 ~= a) or (a ~= b) then
          breakX(rect,floor((x1+x2)/2))
          return
        end
      end

      local interiorPix = math.max(0,(x2-x1 - 1)) * math.max(0,(y2-y1 - 1))
      if interiorPix > 0 then
        pixelsDone = pixelsDone + interiorPix
      end
      if T1 ~= -1 then
        local c = gradient[(T1)%(360*smoothMult)]
        setRect(x1,y1,x2-x1+1,y2-y1+1,0xFF000000 + c)
        ops = ops + 1
      else
        --drawSprite(debugDrawY,x1,y1,x2-x1,y2-y1)
      end

    else
      -- break apart on long axis (if can be broken)
      if (T1 ~= T2) or (T1 ~= T3) or (T1 ~= T4) then
        breakY(rect,floor((y1+y2)/2))
        return
      end

      local icx1, icx2 = iterCount[x1], iterCount[x2]
      for y = y1+1,y2-1 do
        local a = icx1[y] or pix(x1,y)
        local b = icx2[y] or pix(x2,y)
        if (T1 ~= a) or (a ~= b) then
          breakY(rect,floor((y1+y2)/2))
          return
        end
      end

      for x = x1+1,x2-1 do
        local a = iterCount[x][y1] or pix(x,y1)
        local b = iterCount[x][y2] or pix(x,y2)
        if (T1 ~= a) or (a ~= b) then
          breakY(rect,floor((y1+y2)/2))
          return
        end
      end

      local interiorPix = math.max(0,(x2-x1 - 1)) * math.max(0,(y2-y1 - 1))
      if interiorPix > 0 then
        pixelsDone = pixelsDone + interiorPix
      end
      if T1 ~= -1 then
        local c = gradient[(T1)%(360*smoothMult)]
        setRect(x1,y1,x2-x1+1,y2-y1+1,0xFF000000 + c)
        ops = ops + 1
      else
        --drawSprite(debugDrawY,x1,y1,x2-x1,y2-y1)
      end

    end
  end

  while rq_E >= rq_S do
      --Rectangles of format {x1,y1,x2,y2}
    local rect = rectangleQueue[rq_S]
    rectangleQueue[rq_S] = nil
    rq_S = rq_S + 1
    processRectangle(rect)
    if ops > opLimit then
      ops = ops - opLimit
      coroutine.yield({ID = "statusRF", p = pixelsDone})
      opLimit = CO_VAR.OP_LIMIT
    end
  end

  if CO_VAR.ANTIALIASING then
    local pointsX = {}
    local pointsY = {}
    local pointsLength = 0
    local gradientRGB = (smooth and gradientSmoothRGB or gradientRGB)

    local threshold = CO_VAR.AA_Threshold
    local abs = math.abs
    local function compareRGB(c1,c2)
      if not c2 then return false end
      if c1 == c2 then return false end
      local C1 = gradientRGB[(c1)%(360*smoothMult)]
      local C2 = gradientRGB[(c2)%(360*smoothMult)]
      return
        abs(C1[1]-C2[1]) > threshold or
        abs(C1[2]-C2[2]) > threshold or
        abs(C1[3]-C2[3]) > threshold
    end
    local min,max = math.min, math.max
    for x = 0,CO_VAR.RES_X-1 do
      local RGB_column = iterCount[x]
      local columnPrev = iterCount[max(x-1,0)]
      local columnNext = iterCount[min(x+1,CO_VAR.RES_X-1)]
      local yAt = RGB_column[0]
      local yPrev = yAt
      local yNext = RGB_column[1]
      local maxY = CO_VAR.RES_Y-1
      for y = 0,maxY do
        if yAt then
          if compareRGB(yAt,yPrev) or
             compareRGB(yAt,yNext) or
             compareRGB(yAt,columnPrev[y]) or
             compareRGB(yAt,columnNext[y]) then
            pointsLength = pointsLength + 1
            pointsX[pointsLength] = x
            pointsY[pointsLength] = y
          end
          ops = ops + 1/12
        end
        ops = ops + 1/64
        if ops > opLimit then
          ops = ops - opLimit
          coroutine.yield({ID = "AAstatus1",x = x, y = y})
          opLimit = CO_VAR.OP_LIMIT
        end
        yPrev = yAt
        yAt = yNext
        yNext = RGB_column[min(y+2,maxY)]
      end
      if x >= 1 then iterCount[x-1] = nil end
    end
    iterCount = nil
    local COPIES = 7 -- copies of the table to go through
    local method = CO_VAR.AA_Method
    local super = CO_VAR.AA_Supersample
    local pointTable = sampleGenerators[method](super,7)
    for index = 1, pointsLength do
      local x, y = pointsX[index], pointsY[index]
      local maxY = CO_VAR.RES_Y
      local red, g, b = 0, 0, 0
      local offsets = pointTable[index%COPIES + 1]
      local offx, offy = offsets.x, offsets.y
      for p = 1,super do
        local r = CO_VAR.R_BASE + (x + offx[p])*CO_VAR.SCALE
        local i = CO_VAR.I_BASE + (maxY-y + offy[p])*CO_VAR.SCALE
        local iterations = 0
        local zr, zi, cr, ci = r, i, r, i
        --local z = new(r,i)
        --local c = new(r,i)
        while (zr*zr + zi*zi) < bailout and iterations < maxIterations do
          --z = add(square(z),c)
          zr,zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
          iterations = iterations + 1
        end
        local col = ZeroRGB
        ops = ops + iterations/iterPerOp
        if (zr*zr + zi*zi) < bailout and iterations >= maxIterations then 
          iterations = -1 -- Infinity
        elseif not smooth then
          col = gradientRGB[(iterations*iterMult)%360]
        elseif smooth then
          local abs = (zr*zr + zi*zi)^0.5
          local iterSmooth = iterations + il*lp - il*log(log(abs))
          col = getGradientColorAtRGB((iterSmooth*iterMult)%360)
        end
        red, g, b = red + col[1]^2, g + col[2]^2, b + col[3]^2
        if ops > opLimit then
          ops = ops - opLimit
          coroutine.yield({ID = "AAstatus2",i = index,max = pointsLength})
          opLimit = CO_VAR.OP_LIMIT
        end
      end
      setPixel(x,y,0xFF000000 + utils.RGBtoHex((red/super) ^ (1/2),(g/super) ^ (1/2),(b/super) ^ (1/2)))
      ops = ops + 1
    end
  end
  coroutine.yield({ID = "finish", time = tolua(game.elapsedMS) - startTime})
end

local dataHandlers = {}

game.start.addListener(function()
xpcall(function()
  local ResolutionScale = 1
  local PreResolutionScale = 1
  local RenderOption = 3
  local layerBase = tolua(game.level.newArtLayer(1))
  layerBase.layerNum = 4
  CO_VAR.layerBase = layerBase

  event.registerEvent("startRender")

  local rendererOptions = {
    [1] = renderSet,
    [2] = renderSet2,
    [3] = renderSet3
  }
  local jobs = {}
  event.addListener("startRender",function()
    CO_VAR.SCALE = CO_VAR.SCALE * (ResolutionScale/PreResolutionScale)
    ResolutionScale = PreResolutionScale
    CO_VAR.RES_X = 675 * ResolutionScale
    CO_VAR.RES_Y = 480 * ResolutionScale
    player.fov = 1/ResolutionScale
    player.camerax = 8.4375 * (ResolutionScale-1)
    player.cameray = 6 * (ResolutionScale-1)
    
    local renderer = rendererOptions[RenderOption]
    jobs["render"] = coroutine.create(function()
      renderer()
    end)
  end)

  local opsControl
  do
    local TARGET_MS = 50 -- target this frame time
    local OPS_CHANGE = 0.05 -- how much to change per frame.
    local DROP_THRESHOLD = 15 -- immediately drop op count far if this much higher
    local opsTarget = CO_VAR.OP_LIMIT
    local lastMS = tolua(game.elapsedRealMS)
    opsControl = function()
      local thisMS = tolua(game.elapsedRealMS)
      local diff = math.max(10,thisMS-lastMS)
      lastMS = thisMS
      local oL = CO_VAR.OP_LIMIT
      if diff > (TARGET_MS * DROP_THRESHOLD) then
        oL = oL / 3
        opsTarget = oL
      else
        opsTarget = oL / (diff/TARGET_MS)
      end
      oL = math.lerp(oL,opsTarget,OPS_CHANGE)
      CO_VAR.OP_LIMIT = math.floor(oL*100)/100
      --player.chat("Ops: " .. oL)
    end
  end

  player.newTimer(1,1,function()
    player.minimap = false
    player.speed = -50
    player.accel = 100
    player.stiffness = 0
    player.camerax = 0
    player.cameray = 0
    player.chat("When the fractal is done rendering, use ctrl + H to check out the controls!")
  end)

  game.newRealTimer(2,-1,function(timer)
    xpcall(function() -- xpcall for safety, since otherwise an error here crashes pr3.
      local jobCount = 0
      for i in pairs(jobs) do
        jobCount = jobCount + 1
        local co = jobs[i]
        local _, a = coroutine.resume(co)
        if type(a) == "table" then
          local id = i .. ":" .. a.ID
          if dataHandlers[id] then
            dataHandlers[id](a)
          end
        elseif not _ then
          player.chat(a)
        end
        if coroutine.status(co) == "dead" then
          jobs[i] = nil
          player.alert("Finished \"" .. i .. "\"")
        end
      end
      if jobCount > 0 then -- ops control
        opsControl()
      end
    end, function(err)
      player.chat(err,0xFF0000)
    end)
    timer.elapsedMS = 0
  end)

  local keyPressed = tolua(player.keypressed)
  local function keyDown(key)
    return tolua(keyPressed(key))
  end

  local case = "none"
  local enter = {} -- for entering a case
  local cases = {}


  do -- case zoom (harder)
    local enterZoomLayer = tolua(game.level.newArtLayer(1))
    enterZoomLayer.layerNum = 5

    local zSprite = tolua(game.level.newSprite())
    zSprite.lineStyle(0xA0C0C0C0,2,toobject{
      pixelHinting = true,
      scaleMode = "none",
      caps = "square",
      joints = "miter",
    })
    zSprite.drawRect(0,0,CO_VAR.RES_X,CO_VAR.RES_Y)

    local x, y = 0, CO_VAR.RES_Y
    local nScale = 1

    local oldWindow = {}
    event.addListener("startRender",function()
      oldWindow.R_BASE = CO_VAR.R_BASE
      oldWindow.I_BASE = CO_VAR.I_BASE
      oldWindow.SCALE = CO_VAR.SCALE
      x,y = 0, CO_VAR.RES_Y
      nScale = 1
      enterZoomLayer.clear()
    end)

    enter["zoom"] = function()
      zSprite.clear()
      zSprite.lineStyle(0xA0C0C0C0,2,toobject{
        pixelHinting = true,
        scaleMode = "none",
        caps = "square",
        joints = "miter",
      })
      zSprite.drawRect(0,0,CO_VAR.RES_X,CO_VAR.RES_Y)
      player.chat("Entering zoom mode.",0xFFFFFF)
    end

    cases["zoom"] = function()
      local update = false
      local speed = keyDown(keys.SHIFT) and 4 or 1
      local speedR = keyDown(keys.SHIFT) and 4*ResolutionScale or 1
      if keyDown(keys.LEFT) or keyDown(keys.A) then
        x = x - speedR; update = true;
      elseif keyDown(keys.RIGHT) or keyDown(keys.D) then
        x = x + speedR; update = true;
      end
      if keyDown(keys.UP) or keyDown(keys.W) then
        y = y - speedR; update = true;
      elseif keyDown(keys.DOWN) or keyDown(keys.S) then
        y = y + speedR; update = true;
      end
      if keyDown(keys.ADD) or keyDown(221) then
        local os = nScale
        nScale = nScale / (1 + .01*speed); update = true;
        x = x + (os-nScale)*CO_VAR.RES_X*0.5
        y = y - (os-nScale)*CO_VAR.RES_Y*0.5
      elseif keyDown(keys.SUBTRACT) or keyDown(219) then
        local os = nScale
        nScale = nScale * (1 + .01*speed); update = true;
        x = x + (os-nScale)*CO_VAR.RES_X*0.5
        y = y - (os-nScale)*CO_VAR.RES_Y*0.5
      end
      if update then
        local alpha = 1
        if nScale > 1 then alpha = alpha - nScale + 1 end
        enterZoomLayer.alpha = alpha
        enterZoomLayer.clear()
        if alpha <= 0 then return end
        enterZoomLayer.drawSprite(zSprite,x,y,nScale,-nScale)
      end
      if keyDown(keys.SPACE) then
        case = "none"
        local oldScale = oldWindow.SCALE
        CO_VAR.R_BASE = oldWindow.R_BASE + (x*oldScale)
        CO_VAR.I_BASE = oldWindow.I_BASE + ((CO_VAR.RES_Y-y)*oldScale)
        CO_VAR.SCALE = oldScale*nScale
        local str = "Set zoom window to: \n" ..
        "RE: (" .. CO_VAR.R_BASE .. ", " .. CO_VAR.R_BASE+CO_VAR.SCALE*CO_VAR.RES_X .. ")\n" ..
        "IM: (" .. CO_VAR.I_BASE .. ", " .. CO_VAR.I_BASE+CO_VAR.SCALE*CO_VAR.RES_Y .. ")\n"
        player.chat(str,0xFF8000)
      end
    end

  end

  do -- case iterations (easier)
    local iterTextLayer = tolua(game.level.newArtLayer(1))
    iterTextLayer.layerNum = 5
    iterTextLayer.alpha = 0
    local textSprite = tolua(game.level.newSprite())
    textSprite.addText("Max iterations: " .. CO_VAR.ITER_MAX, 240*ResolutionScale, 450*ResolutionScale)
    iterTextLayer.drawSprite(textSprite)

    enter["iterations"] = function()
      iterTextLayer.alpha = 1
    end

    local floor = math.floor
    cases["iterations"] = function()
      local iter = CO_VAR.ITER_MAX
      local speed = keyDown(keys.SHIFT) and 4 or 1
      local uText = false
      if keyDown(keys.UP) or keyDown(keys.W) then
        iter = floor(0.5 + math.max(iter*(1+0.01*speed),iter+speed))
        uText = true
      elseif keyDown(keys.DOWN) or keyDown(keys.S) then
        iter = floor(0.5 + math.min(iter/(1+0.01*speed),iter-speed))
        if iter < 2 then 
          iter = 2
        else
          uText = true
        end
      end

      if uText then -- need to update text
        textSprite.clear()
        textSprite.addText("Max iterations: " .. iter, 240*ResolutionScale, 450*ResolutionScale)
        iterTextLayer.clear()
        iterTextLayer.drawSprite(textSprite)
        CO_VAR.ITER_MAX = iter
      end

      if keyDown(keys.SPACE) then 
        case = "none" 
        player.chat("Set max iteration count to " .. iter,0xFF00FF)
        iterTextLayer.alpha = 0
      end
    end
  end

  do -- case colors (easiest)
    local CTextLayer = tolua(game.level.newArtLayer(1))
    CTextLayer.layerNum = 5
    CTextLayer.alpha = 0
    local textSprite = tolua(game.level.newSprite())
    textSprite.addText("Smoothing: false\nColor multiplier: 4", 240*ResolutionScale, 430*ResolutionScale)
    CTextLayer.drawSprite(textSprite)
    local lastS = false

    local imult = CO_VAR.ITER_MULT
    
    enter["colors"] = function()
      player.chat("Entered color mode.",0x00FFFF)
      CTextLayer.alpha = 1
      imult = CO_VAR.ITER_MULT
      textSprite.clear()
      textSprite.addText("Smoothing: " .. tostring(CO_VAR.SMOOTH) 
        .. "\nColor multiplier: " .. imult, 240*ResolutionScale, 430*ResolutionScale)
      CTextLayer.clear()
      CTextLayer.drawSprite(textSprite)
    end

    local lastKeyLB = false
    local lastKeyRB = false
    cases["colors"] = function()
      local s = keyDown(keys.S)
      local KeyLB = keyDown(219)
      local KeyRB = keyDown(221)
      local u = false
      if KeyLB and not lastKeyLB then
        imult = math.max(1,imult - 1)
        u = true
      elseif KeyRB and not lastKeyRB then
        imult = imult + 1
        u = true
      end
      lastKeyLB = KeyLB
      lastKeyRB = KeyRB
      if s and not lastS then
        u = true
        lastS = true
        if CO_VAR.SMOOTH then
          CO_VAR.SMOOTH = false
          player.chat("Turned smoothing off.",0xFF00FF)
        else
          CO_VAR.SMOOTH = true
          player.chat("Turned smoothing on.",0xFF00FF)
        end
      elseif not s then
        lastS = false
      end
      if u then
        textSprite.clear()
        textSprite.addText("Smoothing: " .. tostring(CO_VAR.SMOOTH) 
          .. "\nColor multiplier: " .. imult, 240*ResolutionScale, 430*ResolutionScale)
        CTextLayer.clear()
        CTextLayer.drawSprite(textSprite)  
      end
      if keyDown(keys.SPACE) then
        case = "none"
        CO_VAR.ITER_MULT = imult
        CTextLayer.alpha = 0
        player.chat("Left color mode.", 0x00FFFF)
      end
    end
  end

  do -- case resolution (really easy)
    local ResTextLayer = tolua(game.level.newArtLayer(1))
    ResTextLayer.layerNum = 5
    ResTextLayer.alpha = 0
    local textSprite = tolua(game.level.newSprite())
    textSprite.addText("Resolution: Standard (675*480)", 240*ResolutionScale, 450*ResolutionScale)
    ResTextLayer.drawSprite(textSprite)
    local Res_Presets = {
      {"Standard",1},
      {"High",2},
      {"Very High",3},
      {"Extreme",4},
      {"Insane",6},
      {"What the fuck", 8},
      {"If pr3 crashes it's your fault", 12}
    }

    local Res_Preset = 1
    local oldRes_Preset = 1
    enter["resolution"] = function()
      ResTextLayer.alpha = 1
      local p = Res_Presets[Res_Preset]
      textSprite.clear()
      textSprite.addText("Resolution: " .. p[1] .. " (".. p[2]*675 .. "x" .. p[2]*480 .. ")", 240*ResolutionScale, 450*ResolutionScale)
      ResTextLayer.clear()
      ResTextLayer.drawSprite(textSprite)
      player.chat("Entered resolution settings.",0x4040FF)
    end
    local lastKeyLB = false
    local lastKeyRB = false
    cases["resolution"] = function()
      local KeyLB = keyDown(219)
      local KeyRB = keyDown(221)
      if KeyLB and not lastKeyLB then
        Res_Preset = math.max(Res_Preset-1,1)
      elseif KeyRB and not lastKeyRB then
        Res_Preset = math.min(Res_Preset+1,#Res_Presets)
      end
      lastKeyLB = KeyLB; lastKeyRB = KeyRB;
      if Res_Preset ~= oldRes_Preset then
        oldRes_Preset = Res_Preset
        local p = Res_Presets[Res_Preset]
        textSprite.clear()
        textSprite.addText("Resolution: " .. p[1] .. " (".. p[2]*675 .. "x" .. p[2]*480 .. ")", 240*ResolutionScale, 450*ResolutionScale)
        ResTextLayer.clear()
        ResTextLayer.drawSprite(textSprite)
        PreResolutionScale = p[2]
      end
      if keyDown(keys.SPACE) then
        case = "none"
        ResTextLayer.alpha = 0
        player.chat("Left resolution settings.",0x4040FF)
      end
    end

  end

  do -- case renderer (really easy)
    local RTextLayer = tolua(game.level.newArtLayer(1))
    RTextLayer.layerNum = 5
    RTextLayer.alpha = 0
    local textSprite = tolua(game.level.newSprite())
    textSprite.addText("Renderer: Rectangle Tracing", 240*ResolutionScale, 450*ResolutionScale)
    RTextLayer.drawSprite(textSprite)
    local R_Presets = {
      {"Rectangle Tracing",[[
       Generally the fastest
       Skips iterations by tracing rectangles and
       skipping the interior if color is the same.
       Also optimizes drawing in the same way.
       Can skip some extra iterations with period checking.
      ]],3},
      {"Simple",[[
       Very fast with lower iteration counts.
       Does not ever skip iterations, so can become
       very slow with large areas of high iterations.
       Optimizes drawing by drawing lines of 
       same-color pixels all at once.
      ]],1},
      {"Floodfill",[[
       Slowest in most situations, as it does not optimize drawing.
       Skips almost all iteration in interior portion of set.
       Can only draw a pixel at a time.
       May be fastest in some situations, such as extremely
       high iteration limits.
      ]],2},
    }

    local R_Preset = 1
    local oldR_Preset = 1
    enter["rendering"] = function()
      RTextLayer.alpha = 1
      local p = R_Presets[R_Preset]
      textSprite.clear()
      textSprite.addText("Renderer: " .. p[1] .. "\n" .. p[2], 180*ResolutionScale, 360*ResolutionScale)
      RTextLayer.clear()
      RTextLayer.drawSprite(textSprite)
      player.chat("Entered renderer settings.",0xFFFFFF)
    end
    local lastKeyLB = false
    local lastKeyRB = false
    cases["rendering"] = function()
      local KeyLB = keyDown(219)
      local KeyRB = keyDown(221)
      if KeyLB and not lastKeyLB then
        R_Preset = math.max(R_Preset-1,1)
      elseif KeyRB and not lastKeyRB then
        R_Preset = math.min(R_Preset+1,#R_Presets)
      end
      lastKeyLB = KeyLB; lastKeyRB = KeyRB;
      if R_Preset ~= oldR_Preset then
        oldR_Preset = R_Preset
        local p = R_Presets[R_Preset]
        textSprite.clear()
        textSprite.addText("Renderer: " .. p[1] .. "\n" .. p[2], 180*ResolutionScale, 360*ResolutionScale)
        RTextLayer.clear()
        RTextLayer.drawSprite(textSprite)
        RenderOption = p[3]
      end
      if keyDown(keys.SPACE) then
        case = "none"
        RTextLayer.alpha = 0
        player.chat("Left renderer settings.",0xFFFFFF)
      end
    end

  end
  do -- case aa (pretty easy)
    local AATextLayer = tolua(game.level.newArtLayer(1))
    AATextLayer.layerNum = 5
    AATextLayer.alpha = 0
    local textSprite = tolua(game.level.newSprite())
    textSprite.addText("", 240*ResolutionScale, 450*ResolutionScale)
    AATextLayer.drawSprite(textSprite)
    local AAPreviewLayer = tolua(game.level.newArtLayer(1))
    AAPreviewLayer.layerNum = 6
    AAPreviewLayer.alpha = 0
    local AAPreviewBase = tolua(game.level.newSprite())
    AAPreviewBase.lineStyle(0x80FFFFFF,2,toobject{pixelHinting = true, joints = "miter", caps = "square"})
    AAPreviewBase.beginFill(0x80000000)
    AAPreviewBase.drawRect(-32,-32,64,64)
    AAPreviewBase.endFill()
    local AA_SampleMethods = {
      [0] = {"None",[[
       No anti-aliasing.
       Fast, but noisy image quality.
      ]]},
      {"Grid",[[
       Simple grid sample method.
       Sample number restricted to square numbers.
       Completely uniform sampling.
       Poor quality on grid-aligned components.
      ]]},
      {"Random",[[
       Randomly picks n points within pixel to sample.
       Can have any sample number.
       No guarantee of uniform sampling.
       Avoids regularity of some other methods.
      ]]},
      {"Jitter",[[
       Randomly picks points within n subpixels
       of a pixel to sample.
       Sample number restricted to square numbers.
       More likely to be uniform than random.
      ]]},
      {"N-Rooks",[[
       Picks n points on (n*n)-grid with none
       on the same row or column.
       Can have any sample number.
       Avoids regularity of other methods.
      ]]},
    }
    local AA_SampleRestrict = {
      ["Grid"] = function(n) return math.floor(n^0.5)^2 end,
      ["Jitter"] = function(n) return math.floor(n^0.5)^2 end,
    }
    local lastAA_Method = 1
    local AA_Method = 0
    local AA_Threshold = 0
    local lastAA_Threshold = 0
    local AA_Sample = 1
    local lastAA_Sample = 1
    enter["aa"] = function()
      AATextLayer.alpha = 1
      AA_Threshold = CO_VAR.AA_Threshold
      lastAA_Threshold = AA_Threshold
      AA_Sample = CO_VAR.AA_Supersample
      lastAA_Sample = AA_Sample
      AAPreviewLayer.clear()
      player.chat("Entered anti-aliasing settings.",0xFF8000)
    end

    local function round(x,n)
      return math.floor(x*10^n + 0.5)/10^n
    end

    local lastKeyLB = false
    local lastKeyRB = false
    local lastKeyP = false
    cases["aa"] = function()
      local KeyLB = keyDown(219)
      local KeyRB = keyDown(221)
      if KeyLB and not lastKeyLB then
        AA_Method = math.max(AA_Method-1,0)
      elseif KeyRB and not lastKeyRB then
        AA_Method = math.min(AA_Method+1,#AA_SampleMethods)
      end
      lastKeyLB = KeyLB
      lastKeyRB = KeyRB

      if keyDown(keys.UP) then
        AA_Threshold = AA_Threshold*1.01
      elseif keyDown (keys.DOWN) then
        AA_Threshold = AA_Threshold/1.01
      end
      if keyDown(keys.LEFT) then
        AA_Sample = math.max(1,(AA_Sample-0.1)/1.02)
      elseif keyDown (keys.RIGHT) then
        AA_Sample = (AA_Sample+0.1)*1.02
      end

      if keyDown(keys.SHIFT) and keyDown(keys.P) and not lastKeyP then
        lastKeyP = true
        local samples = round(AA_Sample,0)
        local method = AA_SampleMethods[AA_Method]
        if AA_SampleRestrict[method[1]] then
          samples = AA_SampleRestrict[method[1]](samples)
        end
        local t = sampleGenerators[method[1]](samples,1)[1]
        local tx,ty = t.x, t.y
        local previewSprite = tolua(game.level.newSprite())
        previewSprite.beginFill(0xFFFFFFFF)
        local circle = tolua(previewSprite.drawCircle)
        for i = 1, #tx do
          local x, y = tx[i]*256*ResolutionScale, ty[i]*256*ResolutionScale
          circle(x,y,2)
        end
        AAPreviewLayer.clear()
        AAPreviewLayer.drawSprite(AAPreviewBase,460*ResolutionScale,160*ResolutionScale,2*ResolutionScale,2*ResolutionScale)
        AAPreviewLayer.drawSprite(previewSprite,460*ResolutionScale,160*ResolutionScale,0.5,0.5)
        AAPreviewLayer.alpha = 1
        previewSprite.destroy()
      elseif not keyDown(keys.P) then
        lastKeyP = false
      end

      if AA_Method ~= lastAA_Method or 
         round(AA_Threshold,3) ~= lastAA_Threshold or
         round(AA_Sample,0) ~= lastAA_Sample then
        lastAA_Threshold = round(AA_Threshold,3)
        lastAA_Method = AA_Method
        lastAA_Sample = round(AA_Sample,0)
        local method = AA_SampleMethods[AA_Method]
        --update preset text
        local text = "Sample Count: " .. lastAA_Sample
        if AA_SampleRestrict[method[1]] then
          local realSamples = AA_SampleRestrict[method[1]](lastAA_Sample)
          text = text .. " (" .. realSamples .. ")"
        end
        text = text .. "      AA Threshold: " .. lastAA_Threshold
        text = text .. "\nAA Sample Method: " .. method[1] 
                     .. "\n" .. method[2]
        textSprite.clear()
        textSprite.addText(text, 220*ResolutionScale, 350*ResolutionScale)
        AATextLayer.clear()
        AATextLayer.drawSprite(textSprite)
      end
      if keyDown(keys.SPACE) then
        case = "none"
        CO_VAR.AA_Threshold = round(AA_Threshold,3)
        if AA_Method == 0 or lastAA_Sample <= 1 then
          CO_VAR.ANTIALIASING = false
        else
          CO_VAR.ANTIALIASING = true
          local method = AA_SampleMethods[AA_Method][1]
          CO_VAR.AA_Method = method
          local samples = lastAA_Sample
          if AA_SampleRestrict[method] then
            samples = AA_SampleRestrict[method](samples)
          end
          CO_VAR.AA_Supersample = samples
        end
        AATextLayer.alpha = 0
        AAPreviewLayer.alpha = 0
        player.chat("Left anti-aliasing settings.", 0xFF8000)
      end
    end

  end

  local lastCtrl = false
  player.tick.addListener(function()
    local c = 0
    for i in pairs(jobs) do c = c + 1 end
    if c ~= 0 then return end -- only run the rest if nothing is happening.

    local shift = keyDown(keys.SHIFT)
    local ctrl = keyDown(keys.CONTROL)
    if shift and ctrl and keyDown(keys.F) then
      player.finish()
      return
    end
    if ctrl and keyDown(keys.H) and not lastCtrl then
      player.alert([[
Controls:

In zoom mode:
  Arrow keys or WASD to move window.
  Plus (numpad) or ] to zoom in.
  Minus (numpad) or [ to zoom out.
  Hold shift to move and zoom faster.
  Press space to confirm zoom and leave zoom mode.

In iteration mode:
  Press up and down (or W and S) to change iteration limit.
  Hold shift to change iteration limit faster.
  (Higher iterations increase detail, but take longer to render)
  Press space to confirm iteration count and leave iteration mode.

In anti-aliasing settings:
  Press up and down to change anti-aliasing threshold.
    A higher threshold can be faster, but might not anti-alias in some places it should.
  Press [ and ] to change between sample patterns.
    Some have better properties than others.
  Press left and right to change sample count.
    More samples takes longer, but improves anti-aliasing quality.
    Higher sample counts have diminishing returns.
    Some sample patterns place limits on sample count.
  Press space to confirm settings and leave anti-aliasing settings.

In resolution settings:
  Press [ and ] to change resolution between presets.
  Note that high resolutions may cause the default preloader to crash.
  It is recommended to use the 64-bit preloader at very high resolutions.

In rendering settings:
  Press [ and ] to switch between the renderers.
  Different renderers should produce identical results.
  If they do not, this is probably a bug.
  Primary differences between renderers are how they draw and their performance.
  

Otherwise:
  Press shift+Z to enter zoom mode.
  Press shift+I to enter iteration mode.
  Press shift+A to enter anti-aliasing settings.
  Press shift+R to enter resolution settings.
  Press shift+E to enter rendering settings.
  Press shift+space to re-render the set using new settings.
  Press ctrl+shift+F to finish.
]])
      lastCtrl = true
    elseif lastCtrl and not ctrl then
      lastCtrl = false
    end
    if cases[case] then
      xpcall(
        cases[case],
        function(err) player.chat(err,0xFF0000) end
      )
    elseif shift then
      -- Allow entering cases.
      if keyDown(keys.I) then
        case = "iterations"
      elseif keyDown(keys.Z) then
        case = "zoom"
      elseif keyDown(keys.C) then
        case = "colors"
      elseif keyDown(keys.A) then
        case = "aa"
      elseif keyDown(keys.R) then
        case = "resolution"
      elseif keyDown(keys.E) then
        case = "rendering"
      elseif keyDown(keys.SPACE) then
        player.chat("Starting new render.",0xFFFFFF)
        CO_VAR.layerBase.clear()
        event.doEvent("startRender")
        return
      end
      if enter[case] then enter[case]() end
    end
  end)

  do
    local total = (CO_VAR.RES_X +1) * (CO_VAR.RES_Y+1)
    local wait = 250 -- status every this many milliseconds
    local lastUpdate = 0
    local statusSprite = tolua(game.level.newSprite())
    local statusLayer = tolua(game.level.newArtLayer(1))
    statusLayer.layerNum = 5
    dataHandlers["render:status"] = function(a)
      local t = tolua(game.elapsedMS)
      if (t - lastUpdate) > wait then
        total = (CO_VAR.RES_X +1) * (CO_VAR.RES_Y+1)
        lastUpdate = lastUpdate + wait
        statusSprite.clear()
        local done = (CO_VAR.RES_Y+1)*a.x + a.y
        statusSprite.addText("Status: " .. done .. " / " .. total, 240, 450)
        statusLayer.clear()
        statusLayer.drawSprite(statusSprite,0,0,ResolutionScale,ResolutionScale)
      end
    end
    --{ID = "statusRF", p = pixelsDone}
    dataHandlers["render:statusRF"] = function(a)
      local t = tolua(game.elapsedMS)
      if (t - lastUpdate) > wait then
        total = (CO_VAR.RES_X) * (CO_VAR.RES_Y)
        lastUpdate = lastUpdate + wait
        statusSprite.clear()
        local done = a.p
        statusSprite.addText("Status: " .. done .. " / " .. total, 240, 450)
        statusLayer.clear()
        statusLayer.drawSprite(statusSprite,0,0,ResolutionScale,ResolutionScale)
      end
    end

    --{ID = "statusFF", qs = rQ_Start, qe = rQ_End}
    dataHandlers["render:statusFF"] = function(a)
      local t = tolua(game.elapsedMS)
      if (t - lastUpdate) > wait then
        statusSprite.clear()
        statusSprite.addText("Status: " .. a.qs .. ", " .. a.qe , 240, 450)
        statusLayer.clear()
        statusLayer.drawSprite(statusSprite,0,0,ResolutionScale,ResolutionScale)
      end
    end

    dataHandlers["render:AAstatus1"] = function(a)
      local t = tolua(game.elapsedMS)
      if (t - lastUpdate) > wait then
        lastUpdate = lastUpdate + wait
        statusSprite.clear()
        local done = (CO_VAR.RES_Y+1)*a.x + a.y
        statusSprite.addText("Status: " .. done .. " / " .. total, 240, 450)
        statusLayer.clear()
        statusLayer.drawSprite(statusSprite,0,0,ResolutionScale,ResolutionScale)
      end
    end

    dataHandlers["render:AAstatus2"] = function(a)
      local t = tolua(game.elapsedMS)
      if (t - lastUpdate) > wait then
        lastUpdate = lastUpdate + wait
        statusSprite.clear()
        local done = a.i
        statusSprite.addText("Status: " .. done .. " / " .. a.max, 240, 450)
        statusLayer.clear()
        statusLayer.drawSprite(statusSprite,0,0,ResolutionScale,ResolutionScale)
      end
    end

    dataHandlers["render:finish"] = function(a)
      statusLayer.clear()
      player.alert("Finished in " .. math.floor(a.time)/1000 .. " seconds!")
    end
  end

  event.doEvent("startRender")
end,function(err) player.alert(err) end)
end)