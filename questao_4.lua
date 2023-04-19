local coroutine = require "coroutine"

local function random_attack()
    local attack_number = math.random(1, 20)
    if attack_number >= 1 and attack_number <= 10 then
        return "Choque do trovão", 50
    elseif attack_number >= 11 and attack_number <= 15 then
        return "Calda de ferro", 100
    elseif attack_number >= 16 and attack_number <= 18 then
        return "Investida Trovão", 150
    else
        return "Trovão", 200
    end
end

local function pikachu_attack(pikachu_hp, raichu_hp)
    local turn = 0
    while true do
        turn = turn + 1
        local attack_name, damage = random_attack()
        raichu_hp = raichu_hp - damage
        print("Turno", turn, "Pikachu usou", attack_name, "causando", damage, "de dano.")
        if raichu_hp <= 0 then
            return 0
        end
        coroutine.yield(raichu_hp)
    end
end

local function raichu_attack(pikachu_hp, raichu_hp)
    local turn = 0
    while true do
        turn = turn + 1
        local attack_name, damage = random_attack()
        pikachu_hp = pikachu_hp - damage
        print("Turno", turn, "Raichu usou", attack_name, "causando", damage, "de dano.")
        if pikachu_hp <= 0 then
            return 0
        end
        coroutine.yield(pikachu_hp)
    end
end

local pikachu_coroutine = coroutine.create(pikachu_attack)
local raichu_coroutine = coroutine.create(raichu_attack)

math.randomseed(os.time())

local pikachu_hp = 800
local raichu_hp = 1000

local turn = 0
while coroutine.status(pikachu_coroutine) ~= "dead" and coroutine.status(raichu_coroutine) ~= "dead" do
    turn = turn + 1
    if turn % 2 == 1 then
        _, raichu_hp = coroutine.resume(pikachu_coroutine, pikachu_hp, raichu_hp)
    else
        _, pikachu_hp = coroutine.resume(raichu_coroutine, pikachu_hp, raichu_hp)
    end
    print("Pikachu HP:", pikachu_hp, "Raichu HP:", raichu_hp)
end

if pikachu_hp <= 0 then
    print("Raichu ganhou a batalha!")
elseif raichu_hp <= 0 then
    print("Pikachu ganhou a batalha!")
end
