$(document).ready(function(){var prototype=$(".participant_input_proto:first"),wrapper=$(".participant_input_wrapper"),add_button=$(".add_field_button");prototype.hide();var init_n_fields=0;for(i=0;i<init_n_fields;i++)add_participant_input();function add_participant_input(){$(wrapper).append(prototype.clone().removeClass("participant_input_proto").show())};$(add_button).click(function(e){e.preventDefault();add_participant_input()});$(wrapper).on("click",".remove_field",function(e){e.preventDefault();$(this).parent('span').remove()})})
