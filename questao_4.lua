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

local function battle(pikachu_hp, raichu_hp)
    local turn = 0

    while pikachu_hp > 0 and raichu_hp > 0 do
        turn = turn + 1
        local attack_name, damage

        if turn % 2 == 1 then
            attack_name, damage = random_attack()
            raichu_hp = raichu_hp - damage
            print("Turno", turn, "Pikachu usou", attack_name, "causando", damage, "de dano.")
        else
            attack_name, damage = random_attack()
            pikachu_hp = pikachu_hp - damage
            print("Turno", turn, "Raichu usou", attack_name, "causando", damage, "de dano.")
        end

        print("Pikachu HP:", pikachu_hp, "Raichu HP:", raichu_hp)
        
        if pikachu_hp <= 0 then
            print("Raichu ganhou a batalha!")
        elseif raichu_hp <= 0 then
            print("Pikachu ganhou a batalha!")
        end
        
        coroutine.yield()
    end
end

local battle_coroutine = coroutine.create(battle)

math.randomseed(os.time())

local pikachu_hp = 800
local raichu_hp = 1000

coroutine.resume(battle_coroutine, pikachu_hp, raichu_hp)
while coroutine.status(battle_coroutine) ~= "dead" do
    coroutine.resume(battle_coroutine)
end